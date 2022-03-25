
module Eclair.Lowering.RA ( compileToEIR ) where

import Protolude hiding (Type)
import Control.Arrow ((&&&))
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.Functor.Foldable
import qualified Eclair.RA.IR as RA
import qualified Eclair.EIR.IR as EIR
import Eclair.RA.IR (RA)
import Eclair.EIR.IR (EIR)
import Eclair.RA.IndexSelection
import Eclair.Syntax (Id(..))
import qualified Eclair.Runtime.Metadata as M
import Eclair.TypeSystem
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Eclair.EIR.Codegen
-- TODO: cleanup imports

compileToEIR :: TypeInfo -> RA -> EIR
compileToEIR typeInfo ra =
  let (indexMap, getIndexForSearch) = runIndexSelection ra
      containerInfos = getContainerInfos indexMap typeInfo
      end = "the.end"
      lowerState = LowerState typeInfo indexMap getIndexForSearch containerInfos end mempty
      initFn = compileInit containerInfos
      destroyFn = compileDestroy containerInfos
      runFn = compileRun lowerState ra
      moduleStmts =
        [ EIR.DeclareType $ map (\(_, _, m) -> m) containerInfos
        , initFn
        , destroyFn
        , runFn
        -- TODO: functions to read/write values,
        -- for each relation? "generateFnsForRelations"
        ]
   in EIR.Block moduleStmts

getContainerInfos :: IndexMap -> TypeInfo -> [ContainerInfo]
getContainerInfos indexMap typeInfo = containerInfos
  where
    combinations r idxs =
      (r,) <$> Set.toList idxs
    toContainerInfo r idx =
      let meta = M.mkMeta idx $ fromJust $ Map.lookup r typeInfo
       in (r, idx, meta)
    storesList = Map.foldMapWithKey combinations indexMap
    containerInfos = map (uncurry toContainerInfo) storesList

forEachRelation :: EIR -> [ContainerInfo] -> (ContainerInfo -> EIR -> EIR) -> EIR
forEachRelation programPtr containerInfos f = EIR.Block stmts
  where
    fieldIndices = zip [0..] containerInfos
    stmts = map (uncurry call) fieldIndices
    call fieldOffset containerInfo =
      let relationPtr = EIR.FieldAccess programPtr fieldOffset
      in f containerInfo relationPtr

compileInit :: [ContainerInfo] -> EIR
compileInit containerInfos =
  let programPtr = EIR.Var "program"
   in EIR.Function "eclair_program_init" [] $ EIR.Block
        [ EIR.Assign programPtr $ EIR.HeapAllocateProgram
        , forEachRelation programPtr containerInfos $ \_ci relationPtr ->
            EIR.Call EIR.InitializeEmpty [relationPtr]
        -- Open question: if some facts are known at compile time, search for derived facts up front?
        , EIR.Return programPtr
        ]

compileDestroy :: [ContainerInfo] -> EIR
compileDestroy containerInfos =
  let programPtr = EIR.FunctionArg 0
   in EIR.Function "eclair_program_destroy" [EIR.Pointer EIR.Program] $ EIR.Block
        [ forEachRelation programPtr containerInfos $ \_ci relationPtr ->
            EIR.Call EIR.Destroy [relationPtr]
        , EIR.FreeProgram programPtr
        ]

compileRun :: LowerState -> RA -> EIR
compileRun lowerState ra =
  let end = endLabel lowerState
  in runCodegenM lowerState $ do
       stmts <- generateProgramInstructions ra
       pure $ EIR.Function "eclair_program_run" [EIR.Pointer EIR.Program] $
         EIR.Block [ stmts, EIR.Jump end, EIR.Label end ]

-- TODO: use emit iso wrapping in Block? -> yes
generateProgramInstructions :: RA -> CodegenM EIR
generateProgramInstructions = zygo constraintsForSearch $ \case
  RA.ModuleF (map snd -> actions) -> EIR.Block <$> sequence actions
  RA.ParF (map snd -> actions) -> EIR.Par <$> sequence actions
  RA.SearchF r alias clauses (snd -> action) -> do
    (idx, columns) <- idxFromConstraints r (concatMap fst clauses)
    relationPtr <- lookupRelationByIndex r idx
    lbValue <- initValue r LowerBound columns
    ubValue <- initValue r UpperBound columns
    let query = List.foldl1' EIR.And <$> sequence (map snd clauses)
    rangeQuery r relationPtr lbValue ubValue $ \iter -> do
      let currValue = EIR.Call EIR.IterCurrent [iter]
      --  TODO: use non empty?
      case length clauses of
        0 -> -- No query to check: always matches
          withUpdatedAlias alias currValue action
        _ -> do
          isMatch <- withSearchState alias currValue query
          -- TODO: check if this works
          EIR.If isMatch <$> withUpdatedAlias alias currValue action
  RA.ProjectF r (map snd -> unresolvedValues) -> do
    -- NOTE: Value type is the same for all (but not the insert function)
    values <- withProjectState r $ sequence unresolvedValues
    indices <- toList . fromJust . Map.lookup r . idxMap <$> getLowerState
    let var = EIR.Var "value"
        value = EIR.Assign var $ EIR.StackAllocate EIR.Value r
        assignStmts = zipWith (EIR.Assign . EIR.FieldAccess var) [0..] values
    insertStmts <- for indices $ \idx -> do
      relationPtr' <- lookupRelationByIndex r idx
      pure $ EIR.Call EIR.Insert [relationPtr', var]
    pure $ EIR.Block $ assignStmts ++ insertStmts
  RA.PurgeF r -> relationUnaryFn r EIR.Purge
  RA.MergeF r1 r2 -> relationBinFn r1 r2 EIR.Merge
  RA.SwapF r1 r2 -> relationBinFn r1 r2 EIR.Swap
  RA.LoopF (map snd -> actions) -> do
    end <- genLabel "loop.end"
    loop <- map EIR.Loop $ withEndLabel end $
      sequence actions
    pure $ EIR.Block [loop, EIR.Label end]
  RA.ExitF rs -> do
    end <- endLabel <$> getLowerState
    foldr' f (EIR.Jump end) <$> traverse getFirstFieldOffset rs
    where
      f field inner =
        let programPtr = EIR.FunctionArg 0
            relationPtr = EIR.FieldAccess programPtr field
            isEmpty = EIR.Call EIR.IsEmpty [relationPtr]
         in EIR.If isEmpty inner
  RA.LitF x -> pure $ EIR.Lit x
  RA.ConstrainF (snd -> lhs) (snd -> rhs) ->
    EIR.Equals <$> lhs <*> rhs
  RA.NotElemF r (map snd -> columnValues) -> do
    let idx = mkFindIndex columnValues
    relationPtr <- lookupRelationByIndex r idx
    columns <- sequence columnValues
    let value = EIR.StackAllocate EIR.Value r
        containsVar = EIR.Var "contains_result"  -- TODO: unique?
    pure $ EIR.Block $ zipWith (EIR.Assign . EIR.FieldAccess value) [0..] columns
                  ++ [ EIR.Assign containsVar $ EIR.Call EIR.Contains [relationPtr, value]
                     , EIR.Not containsVar
                     ]
  RA.ColumnIndexF a' col -> ask >>= \case
    Search a value ls ->
      if a == a'
        then getColumn value col
        else do
          currentAliasValue <- lookupAlias a
          getColumn currentAliasValue col
    Project r ls -> do
      currentAliasValue <- lookupAlias r
      getColumn currentAliasValue col
    Normal _ ->
      panic "Trying to access column index outside of search or project."
    where
      getColumn value col =
        pure $ EIR.FieldAccess value col  -- TODO: check access, see original code

rangeQuery :: Relation -> EIR -> EIR -> EIR -> (EIR -> CodegenM EIR) -> CodegenM EIR
rangeQuery r relationPtr lbValue ubValue loopAction = do
  endLabel <- genLabel "range.end"
  let beginIter = EIR.StackAllocate EIR.Iter r
      endIter = EIR.StackAllocate EIR.Iter r
      initLB = EIR.Call EIR.IterLowerBound [relationPtr, lbValue, beginIter]
      initUB = EIR.Call EIR.IterUpperBound [relationPtr, ubValue, endIter]
      advanceIter = EIR.Call EIR.IterNext [beginIter]
      hasNext = EIR.Not $ EIR.Call EIR.IterIsEqual [beginIter, endIter]
      stopIfFinished = EIR.If hasNext (EIR.Jump endLabel)
      loopStmts = do
        loopBody <- loopAction beginIter
        pure $ [stopIfFinished, loopBody, advanceIter]
  loop <- EIR.Loop <$> loopStmts
  pure $ EIR.Block
    [ beginIter
    , endIter
    , initLB
    , initUB
    , loop
    , EIR.Label endLabel
    ]

data Bound
  = LowerBound
  | UpperBound

initValue :: Relation -> Bound -> [Column] -> CodegenM EIR
initValue r bound columns = do
  typeInfo <- typeEnv <$> getLowerState
  let value = EIR.StackAllocate EIR.Value r
      columnNrs = take (length typeInfo) [0..]
      valuesWithCols = [(nr, x) | nr <- columnNrs, let x = if nr `elem` columns
                                                             then bounded
                                                             else dontCare]
      -- TODO: take actual values into account:
      assignStmts = map (\(i, val) -> EIR.Assign (EIR.FieldAccess value i) val) valuesWithCols
  -- TODO: need emit
  pure $ EIR.Block $ assignStmts ++ [value]
  where
    -- NOTE: only supports unsigned integers for now!
    bounded = EIR.Lit $ case bound of
      LowerBound -> 0
      UpperBound -> 0xffffffff
    dontCare = EIR.Lit 0

relationUnaryFn :: Relation -> EIR.EIRFunction -> CodegenM EIR
relationUnaryFn r fn = map EIR.Block $ forEachIndex r $ \idx -> do
  relationPtr <- lookupRelationByIndex r idx
  pure $ EIR.Call fn [relationPtr]

-- NOTE: assumes r1 and r2 have same underlying representation
-- (guaranteed by earlier compiler stages)
relationBinFn :: Relation -> Relation -> EIR.EIRFunction -> CodegenM EIR
relationBinFn r1 r2 fn = do
  map EIR.Block $ forEachIndex r1 $ \idx -> do
    relation1Ptr <- lookupRelationByIndex r1 idx
    relation2Ptr <- lookupRelationByIndex r2 idx
    pure $ EIR.Call fn [relation1Ptr, relation2Ptr]

forEachIndex :: Relation
             -> (Index -> CodegenM EIR)
             -> CodegenM [EIR]
forEachIndex r f = do
  indexMap <- idxMap <$> getLowerState
  let indices = Set.toList . fromJust $ Map.lookup r indexMap
  traverse f indices

-- Open question: is this index always applicable for find/not elem query?
mkFindIndex :: [a] -> Index
mkFindIndex =
  Index . zipWith const [0..]


{-
-- TODO: name
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
