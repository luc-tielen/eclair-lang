
module Eclair.Lowering.RA ( compileToEIR ) where

import Protolude hiding (Type)
import Protolude.Unsafe (unsafeHead)
import Control.Arrow ((&&&))
import Control.Monad.RWS.Strict
import Data.Maybe (fromJust)
import Data.Functor.Foldable hiding (fold)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Eclair.EIR.Codegen
import Eclair.EIR.IR (EIR)
import Eclair.RA.IR (RA)
import Eclair.RA.IndexSelection
import Eclair.Syntax (Id(..), stripIdPrefixes)
import Eclair.TypeSystem
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.RA.IR as RA
import qualified Eclair.LLVM.Metadata as M


compileToEIR :: TypeInfo -> RA -> EIR
compileToEIR typeInfo ra =
  let (indexMap, getIndexForSearch) = runIndexSelection ra
      containerInfos = getContainerInfos indexMap typeInfo
      end = "the.end"
      lowerState = LowerState typeInfo indexMap getIndexForSearch containerInfos end mempty
      moduleStmts :: [CodegenM EIR]
      moduleStmts =
        [ declareProgram $ map (\(r, _, m) -> (r, m)) containerInfos
        , compileInit
        , compileDestroy
        , compileRun ra
        -- TODO: functions to read/write values,
        -- for each relation? "generateFnsForRelations"
        ]
   in EIR.Block $ map (runCodegen lowerState) moduleStmts

compileInit :: CodegenM EIR
compileInit = do
  program <- var "program"
  initActions <- forEachRelation program $ \(r, idx, _) relationPtr ->
    call r idx EIR.InitializeEmpty [relationPtr]
  fn "eclair_program_init" [] (EIR.Pointer EIR.Program) $
    assign program heapAllocProgram
    : initActions
    -- Open question: if some facts are known at compile time, search for derived facts up front?
    ++ [ ret program ]

compileDestroy :: CodegenM EIR
compileDestroy = do
  let program = fnArg 0
  destroyActions <- forEachRelation program $ \(r, idx, _) relationPtr ->
    call r idx EIR.Destroy [relationPtr]
  fn "eclair_program_destroy" [EIR.Pointer EIR.Program] EIR.Void $
    destroyActions
    ++ [ freeProgram program ]

compileRun :: RA -> CodegenM EIR
compileRun ra = do
  end <- endLabel <$> getLowerState
  fn "eclair_program_run" [EIR.Pointer EIR.Program] EIR.Void
    [generateProgramInstructions ra]

generateProgramInstructions :: RA -> CodegenM EIR
generateProgramInstructions = zygo (combine equalitiesInSearch constraintsForSearch) $ \case
  RA.ModuleF (map snd -> actions) -> block actions
  RA.ParF (map snd -> actions) -> parallel actions
  RA.SearchF r alias clauses (snd -> action) -> do
    let eqsInSearch = foldMap (getNormalizedEqualities . fst . fst) clauses
    let constraints = concatMap (snd . fst) clauses
    idx <- idxFromConstraints r alias constraints
    let relationPtr = lookupRelationByIndex r idx
        query = List.foldl1' and' $ map snd clauses
    (initLBValue, lbValue) <- initValue r idx alias LowerBound eqsInSearch
    (initUBValue, ubValue) <- initValue r idx alias UpperBound eqsInSearch
    block
      [ initLBValue
      , initUBValue
      , rangeQuery r idx relationPtr lbValue ubValue $ \iter -> do
          current <- var "current"
          block
            [ assign current $ call r idx EIR.IterCurrent [iter]
            , do
              currentValue <- current
              case length clauses of
                0 -> -- No query to check: always matches
                  withUpdatedAlias alias currentValue action
                _ -> do
                  withSearchState alias currentValue $
                    withUpdatedAlias alias currentValue $
                      if' query action
            ]
      ]
  RA.ProjectF r (map snd -> unresolvedValues) -> do
    values <- sequence unresolvedValues
    let values' = map pure values
    indices <- indexesForRelation r
    var <- var "value"
    let -- NOTE: for allocating a value, the index does not matter
        -- (a value is always represented as [N x i32] internally)
        -- This saves us doing a few stack allocations.
        firstIdx = unsafeHead indices
        allocValue = assign var $ stackAlloc r firstIdx EIR.Value
        assignStmts = zipWith (assign . fieldAccess var) [0..] values'
        insertStmts = flip map indices $ \idx ->
          -- NOTE: The insert function is different for each r + idx combination though!
          call r idx EIR.Insert [lookupRelationByIndex r idx, var]
    block $ allocValue : assignStmts ++ insertStmts
  RA.PurgeF r ->
    block =<< relationUnaryFn r EIR.Purge
  RA.SwapF r1 r2 ->
    block =<< relationBinFn r1 r2 EIR.Swap
  RA.MergeF r1 r2 -> do
    -- NOTE: r1 = from/src, r2 = to/dst, r1 and r2 have same underlying structure
    indices <- indexesForRelation r1
    block $ flip map indices $ \idx -> do
      beginIter <- var "begin_iter"
      endIter <- var "end_iter"
      let relation1Ptr = lookupRelationByIndex r1 idx
          relation2Ptr = lookupRelationByIndex r2 idx
      block
        [ assign beginIter $ stackAlloc r1 idx EIR.Iter
        , assign endIter $ stackAlloc r1 idx EIR.Iter
        , call r1 idx EIR.IterBegin [relation1Ptr, beginIter]
        , call r1 idx EIR.IterEnd [relation1Ptr, endIter]
        , call r1 idx EIR.InsertRange [relation2Ptr, beginIter, endIter]
        ]
  RA.LoopF (map snd -> actions) -> do
    end <- labelId "loop.end"
    block [withEndLabel end $ loop actions, label end]
  RA.ExitF rs -> do
    end <- endLabel <$> getLowerState
    foldl' f (jump end) =<< traverse getFirstFieldOffset rs
    where
      f inner field = do
        (r, idx, _) <- getContainerInfoByOffset field
        let programPtr = fnArg 0
            relationPtr = fieldAccess programPtr field
            isEmpty = call r idx EIR.IsEmpty [relationPtr]
        if' isEmpty inner
  RA.LitF x -> lit x
  RA.ConstrainF (snd -> lhs) (snd -> rhs) ->
    equals lhs rhs
  RA.NotElemF r (map snd -> columnValues) -> do
    value <- var "value"
    let idx = mkFindIndex columnValues
        relationPtr = lookupRelationByIndex r idx
        allocValue = assign value $ stackAlloc r idx EIR.Value
    containsVar <- var "contains_result"
    let assignActions = zipWith (assign . fieldAccess value) [0..] columnValues
    block $ allocValue : assignActions
         ++ [ assign containsVar $ call r idx EIR.Contains [relationPtr, value]
            , not' containsVar
            ]
  RA.ColumnIndexF a' col -> ask >>= \case
    Search a value ls ->
      if a == a'
        then getColumn value col
        else do
          currentAliasValue <- lookupAlias a'
          getColumn currentAliasValue col
    Normal _ -> do
      currentAliasValue <- lookupAlias a'
      getColumn currentAliasValue col
    where
      getColumn value col =
        fieldAccess (pure value) col

rangeQuery :: Relation
           -> Index
           -> CodegenM EIR
           -> CodegenM EIR
           -> CodegenM EIR
           -> (CodegenM EIR -> CodegenM EIR)
           -> CodegenM EIR
rangeQuery r idx relationPtr lbValue ubValue loopAction = do
  beginIter <- var "begin_iter"
  endIter <- var "end_iter"
  endLabel <- labelId "range_query.end"
  let allocBeginIter = assign beginIter $ stackAlloc r idx EIR.Iter
      allocEndIter = assign endIter $ stackAlloc r idx EIR.Iter
      initLB = call r idx EIR.IterLowerBound [relationPtr, lbValue, beginIter]
      initUB = call r idx EIR.IterUpperBound [relationPtr, ubValue, endIter]
      advanceIter = call r idx EIR.IterNext [beginIter]
      isAtEnd = call r idx EIR.IterIsEqual [beginIter, endIter]
      stopIfFinished = if' isAtEnd (jump endLabel)
      loopStmts = [stopIfFinished, loopAction beginIter, advanceIter]
  block [allocBeginIter, allocEndIter, initLB, initUB, loop loopStmts, label endLabel]

data Bound
  = LowerBound
  | UpperBound

-- NOTE: only supports unsigned integers for now!
initValue :: Relation -> Index -> RA.Alias -> Bound -> [NormalizedEquality] -> CodegenM (CodegenM EIR, CodegenM EIR)
initValue r idx a bound eqs = do
  let r' = stripIdPrefixes r
  typeInfo <- fromJust . Map.lookup r' . typeEnv <$> getLowerState
  value <- var "value"
  let allocValue = assign value $ stackAlloc r idx EIR.Value
      columnNrs = take (length typeInfo) [0..]
      valuesWithCols = [(nr, x) | nr <- columnNrs, let x = if isConstrained nr
                                                              then constrain nr
                                                              else dontCare]
      assignStmts = map (\(i, val) -> assign (fieldAccess value i) val) valuesWithCols
  pure (block $ allocValue : assignStmts, value)
  where
    isConstrained col = any (\(Equality a' col' _) -> a == a' && col == col') eqs
    constrain col =
      let (Equality _ _ val) = fromJust $ find (\(Equality a' col' _) -> a == a' && col == col') eqs
       in case val of
            Constant x -> lit x
            AliasVal a' col' -> fieldAccess (lookupAlias a') col'
    dontCare = lit $ case bound of
      LowerBound -> 0
      UpperBound -> 0xffffffff

combine :: Functor f => (f a -> a) -> (f b -> b) -> f (a, b) -> (a, b)
combine f g = f . fmap fst &&& g . fmap snd

data Val
  = AliasVal RA.Alias Column
  | Constant Int
  deriving Show

data NormalizedEquality
  = Equality RA.Alias Column Val
  deriving Show

type EqSearchM = RWS () [(Val, Val)] (Maybe Val)

equalitiesInSearch :: RA.RAF (EqSearchM ()) -> EqSearchM ()
equalitiesInSearch = \case
  RA.ColumnIndexF a col -> do
    put $ Just $ AliasVal a col
  RA.LitF x -> do
    put $ Just $ Constant x
  RA.ConstrainF lhs rhs -> do
    lhsValue <- lhs *> get
    rhsValue <- rhs *> get
    forM_ ((,) <$> lhsValue <*> rhsValue) $ \eq' ->
      tell [eq']
  raf -> sequence_ raf

getNormalizedEqualities :: EqSearchM a -> [NormalizedEquality]
getNormalizedEqualities m = foldMap normalizeEquality $ snd $ execRWS m () Nothing
  where
    normalizeEquality = \case
      (lhs@(AliasVal a col), rhs@(AliasVal a' col')) ->
        [Equality a col rhs, Equality a' col' lhs]
      (AliasVal a col, x) -> [Equality a col x]
      (x, AliasVal a col) -> [Equality a col x]
      _ -> []

forEachRelation :: CodegenM EIR -> (ContainerInfo -> CodegenM EIR -> CodegenM EIR) -> CodegenM [CodegenM EIR]
forEachRelation program f = do
  cis <- containerInfos <$> getLowerState
  pure $ zipWith doCall [0..] cis
  where
    doCall fieldOffset ci =
      f ci (fieldAccess program fieldOffset)

relationUnaryFn :: Relation -> EIR.Function -> CodegenM [CodegenM EIR]
relationUnaryFn r fn = forEachIndex r $ \idx -> do
  call r idx fn [lookupRelationByIndex r idx]

-- NOTE: assumes r1 and r2 have same underlying representation
-- (guaranteed by earlier compiler stages)
relationBinFn :: Relation -> Relation -> EIR.Function -> CodegenM [CodegenM EIR]
relationBinFn r1 r2 fn = forEachIndex r1 $ \idx -> do
  call r1 idx fn
    [ lookupRelationByIndex r1 idx
    , lookupRelationByIndex r2 idx
    ]

forEachIndex :: Relation
             -> (Index -> CodegenM EIR)
             -> CodegenM [CodegenM EIR]
forEachIndex r f = do
  indices <- indexesForRelation r
  pure $ map f indices

getContainerInfos :: IndexMap -> TypeInfo -> [ContainerInfo]
getContainerInfos indexMap typeInfo = containerInfos
  where
    combinations r idxs =
      (r,) <$> Set.toList idxs
    toContainerInfo r idx =
      let r' = stripIdPrefixes r
          meta = M.mkMeta idx $ fromJust $ Map.lookup r' typeInfo
       in (r, idx, meta)
    storesList = Map.foldMapWithKey combinations indexMap
    containerInfos = map (uncurry toContainerInfo) storesList

-- Open question: is this index always applicable for find/not elem query?
mkFindIndex :: [a] -> Index
mkFindIndex =
  Index . zipWith const [0..]

indexesForRelation :: Relation -> CodegenM [Index]
indexesForRelation r =
  Set.toList . fromJust . Map.lookup r . idxMap <$> getLowerState

{-
generateFnsForRelations :: IndexMap -> TypeInfo -> ModuleBuilderT IO FunctionsMap
generateFnsForRelations indexMap typeInfo = do
  results <- flip Map.traverseWithKey indexMap $ \r idxs -> do
    -- TODO: cache functions if possible?
    -- TODO: avoid codegen collisions between relations
    for (toList idxs) $ \idx -> do
      let meta = mkMeta idx (fromJust $ Map.lookup r typeInfo)
      (idx,) <$> BTree.codegen meta

  pure $ map Map.fromList results
-}
