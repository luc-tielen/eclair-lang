
module Eclair.RA.Lower ( compileToEIR ) where

import Prelude
import Data.Maybe (fromJust)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Eclair.Id
import Eclair.Literal
import Eclair.Comonads hiding (Quad)
import Eclair.TypeSystem
import Eclair.AST.Transforms.ReplaceStrings (StringMap)
import Eclair.RA.Codegen
import Eclair.EIR.IR (EIR)
import Eclair.RA.IR (RA)
import Eclair.RA.IndexSelection
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.RA.IR as RA
import qualified Eclair.LLVM.Metadata as M


compileToEIR :: StringMap -> TypedefInfo -> RA -> EIR
compileToEIR stringMap typedefInfo ra =
  let (indexMap, getIndexForSearch) = runIndexSelection typedefInfo ra
      containerInfos' = getContainerInfos indexMap typedefInfo
      end = "the.end"
      lowerState = LowerState typedefInfo indexMap getIndexForSearch containerInfos' end mempty
      moduleStmts :: [CodegenM EIR]
      moduleStmts =
        [ declareProgram $ map (\(r, _, m) -> (r, m)) containerInfos'
        , compileInit stringMap
        , compileDestroy
        , compileRun ra
        ]
   in EIR.Module $ map (runCodegen lowerState) moduleStmts

compileInit :: StringMap -> CodegenM EIR
compileInit stringMap = do
  program <- var "program"
  let symbolTable = fieldAccess program 0
      symbolTableInitAction = primOp EIR.SymbolTableInit [symbolTable]
  relationInitActions <- forEachRelation program $ \(r, idx, _) relationPtr ->
    call r idx EIR.InitializeEmpty [relationPtr]
  let addSymbolActions = toSymbolTableInsertActions symbolTable stringMap
      initActions = symbolTableInitAction : relationInitActions <> addSymbolActions
  apiFn "eclair_program_init" [] (EIR.Pointer EIR.Program) $
    assign program heapAllocProgram
    : initActions
    -- Open question: if some facts are known at compile time, search for derived facts up front?
    <> [ ret program ]

toSymbolTableInsertActions :: CodegenM EIR -> StringMap -> [CodegenM EIR]
toSymbolTableInsertActions symbolTable stringMap =
  map (doInsert . fst) $ sortWith snd $ Map.toList stringMap
  where
    doInsert symbol =
      primOp EIR.SymbolTableInsert [symbolTable, litStr symbol]
    litStr =
      pure . EIR.Lit . LString


compileDestroy :: CodegenM EIR
compileDestroy = do
  let program = fnArg 0
      symbolTableDestroyAction = primOp EIR.SymbolTableDestroy [fieldAccess program 0]
  relationDestroyActions <- forEachRelation program $ \(r, idx, _) relationPtr ->
    call r idx EIR.Destroy [relationPtr]
  let destroyActions = symbolTableDestroyAction : relationDestroyActions
  apiFn "eclair_program_destroy" [EIR.Pointer EIR.Program] EIR.Void $
    destroyActions
    <> [ freeProgram program ]

compileRun :: RA -> CodegenM EIR
compileRun ra = do
  apiFn "eclair_program_run" [EIR.Pointer EIR.Program] EIR.Void
    [generateProgramInstructions ra]

generateProgramInstructions :: RA -> CodegenM EIR
generateProgramInstructions = gcata (distribute extractEqualities) $ \case
  RA.ModuleF (map extract -> actions) -> block actions
  RA.ParF (map extract -> actions) -> parallel actions
  RA.SearchF r alias clauses (extract -> action) -> do
    let eqsInSearch = foldMap tSnd clauses
        eqs = concatMap normalizedEqToConstraints eqsInSearch
    idx <- idxFromConstraints r alias eqs
    let relationPtr = lookupRelationByIndex r idx
        isConstrain = \case
          RA.CompareOp {} -> True
          _ -> False
        queryClauses = map extract $ filter ((not . isConstrain) . tFst) clauses
        query = List.foldl1' and' queryClauses
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
              case length queryClauses of
                0 -> -- No query to check: always matches
                  withUpdatedAlias alias currentValue action
                _ -> do
                  withSearchState alias currentValue $
                    withUpdatedAlias alias currentValue $
                      if' query action
            ]
      ]
  RA.ProjectF r (map extract -> unresolvedValues) -> do
    values <- sequence unresolvedValues
    let values' = map pure values
    indices <- indexesForRelation r
    var' <- var "value"
    let -- NOTE: for allocating a value, the index does not matter
        -- (a value is always represented as [N x i32] internally)
        -- This saves us doing a few stack allocations.
        firstIdx = fromJust $ viaNonEmpty head indices
        allocValue = assign var' $ stackAlloc r firstIdx EIR.Value
        assignStmts = zipWith (assign . fieldAccess var') [0..] values'
        insertStmts = flip map indices $ \idx ->
          -- NOTE: The insert function is different for each r + idx combination though!
          call r idx EIR.Insert [lookupRelationByIndex r idx, var']
    block $ allocValue : assignStmts <> insertStmts
  RA.PurgeF r ->
    block =<< relationUnaryFn r EIR.Purge
  RA.SwapF r1 r2 ->
    block =<< relationBinFn r1 r2 EIR.Swap
  RA.MergeF r1 r2 -> do
    -- NOTE: r1 = from/src, r2 = to/dst
    -- TODO: which idx? just select first matching? or idx on all columns?
    idxR1 <- fromJust . viaNonEmpty head <$> indexesForRelation r1
    let relation1Ptr = lookupRelationByIndex r1 idxR1

    indices2 <- indexesForRelation r2
    block $ flip map indices2 $ \idxR2 -> do
      beginIter <- var "begin_iter"
      endIter <- var "end_iter"
      let relation2Ptr = lookupRelationByIndex r2 idxR2
      block
        [ assign beginIter $ stackAlloc r1 idxR1 EIR.Iter
        , assign endIter $ stackAlloc r1 idxR1 EIR.Iter
        , call r1 idxR1 EIR.IterBegin [relation1Ptr, beginIter]
        , call r1 idxR1 EIR.IterEnd [relation1Ptr, endIter]
        , call r2 idxR2 (EIR.InsertRange r1 idxR1) [relation2Ptr, beginIter, endIter]
        ]
  RA.LoopF (map extract -> actions) -> do
    end <- labelId "loop.end"
    block [withEndLabel end $ loop actions, label end]
  RA.IfF (extract -> condition) (extract -> action) -> do
    if' condition action
  RA.CompareOpF op (extract -> lhs) (extract -> rhs) -> do
    let toComparison = case op of
          RA.Equals -> equals
          RA.NotEquals -> notEquals
          RA.LessThan -> lessThan
          RA.LessOrEqual -> lessOrEqual
          RA.GreaterThan -> greaterThan
          RA.GreaterOrEqual -> greaterOrEqual
    toComparison lhs rhs
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
  RA.NotElemF r (map extract -> columnValues) -> do
    value <- var "value"
    let idx = mkFindIndex columnValues
        relationPtr = lookupRelationByIndex r idx
        allocValue = assign value $ stackAlloc r idx EIR.Value
    containsVar <- var "contains_result"
    let assignActions = zipWith (assign . fieldAccess value) [0..] columnValues
    block $ allocValue : assignActions
        <> [ assign containsVar $ call r idx EIR.Contains [relationPtr, value]
            , not' containsVar
            ]
  RA.ColumnIndexF a' col -> ask >>= \case
    Search a value _ ->
      if a == a'
        then getColumn value col
        else do
          currentAliasValue <- lookupAlias a'
          getColumn currentAliasValue col
    Normal _ -> do
      currentAliasValue <- lookupAlias a'
      getColumn currentAliasValue col
    where
      getColumn value =
        fieldAccess (pure value)
  where
    distribute :: Corecursive t
               => (Base t (t, a) -> a)
               -> (Base t (Triple t a b) -> Triple t a (Base t b))
    distribute f m =
      let base_t_t = map tFst m
          base_t_ta = map (tFst &&& tSnd) m
          base_t_b = map tThd m
      in Triple (embed base_t_t) (f base_t_ta) base_t_b

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
  endLabel' <- labelId "range_query.end"
  let allocBeginIter = assign beginIter $ stackAlloc r idx EIR.Iter
      allocEndIter = assign endIter $ stackAlloc r idx EIR.Iter
      initLB = call r idx EIR.IterLowerBound [relationPtr, lbValue, beginIter]
      initUB = call r idx EIR.IterUpperBound [relationPtr, ubValue, endIter]
      advanceIter = call r idx EIR.IterNext [beginIter]
      isAtEnd = call r idx EIR.IterIsEqual [beginIter, endIter]
      stopIfFinished = if' isAtEnd (jump endLabel')
      loopStmts = [stopIfFinished, loopAction beginIter, advanceIter]
  block [allocBeginIter, allocEndIter, initLB, initUB, loop loopStmts, label endLabel']

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

forEachRelation :: CodegenM EIR -> (ContainerInfo -> CodegenM EIR -> CodegenM EIR) -> CodegenM [CodegenM EIR]
forEachRelation program f = do
  cis <- containerInfos <$> getLowerState
  pure $ zipWith doCall [1..] cis
  where
    doCall fieldOffset ci =
      f ci (fieldAccess program fieldOffset)

relationUnaryFn :: Relation -> EIR.Function -> CodegenM [CodegenM EIR]
relationUnaryFn r fn' = forEachIndex r $ \idx -> do
  call r idx fn' [lookupRelationByIndex r idx]

-- NOTE: assumes r1 and r2 have same underlying representation
-- (guaranteed by earlier compiler stages)
relationBinFn :: Relation -> Relation -> EIR.Function -> CodegenM [CodegenM EIR]
relationBinFn r1 r2 fn' = forEachIndex r1 $ \idx -> do
  call r1 idx fn'
    [ lookupRelationByIndex r1 idx
    , lookupRelationByIndex r2 idx
    ]

forEachIndex :: Relation
             -> (Index -> CodegenM EIR)
             -> CodegenM [CodegenM EIR]
forEachIndex r f = do
  indices <- indexesForRelation r
  pure $ map f indices

getContainerInfos :: IndexMap -> TypedefInfo -> [ContainerInfo]
getContainerInfos indexMap typedefInfo = containerInfos'
  where
    combinations r idxs =
      (r,) <$> Set.toList idxs
    toContainerInfo r idx =
      let r' = stripIdPrefixes r
          meta = M.mkMeta idx $ fromJust $ Map.lookup r' typedefInfo
       in (r, idx, meta)
    storesList = Map.foldMapWithKey combinations indexMap
    containerInfos' = map (uncurry toContainerInfo) storesList

-- Open question: is this index always applicable for find/not elem query?
mkFindIndex :: [a] -> Index
mkFindIndex =
  Index . zipWith const [0..]

indexesForRelation :: Relation -> CodegenM [Index]
indexesForRelation r =
  Set.toList . fromJust . Map.lookup r . idxMap <$> getLowerState
