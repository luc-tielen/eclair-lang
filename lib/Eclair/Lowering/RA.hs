
module Eclair.Lowering.RA ( compileToEIR ) where

import Protolude hiding (Type)
import Data.Maybe (fromJust)
import Data.Functor.Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Eclair.EIR.Codegen
import Eclair.EIR.IR (EIR)
import Eclair.RA.IR (RA)
import Eclair.RA.IndexSelection
import Eclair.Syntax (Id(..))
import Eclair.TypeSystem
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.RA.IR as RA
import qualified Eclair.Runtime.Metadata as M

compileToEIR :: TypeInfo -> RA -> EIR
compileToEIR typeInfo ra =
  let (indexMap, getIndexForSearch) = runIndexSelection ra
      containerInfos = getContainerInfos indexMap typeInfo
      end = "the.end"
      lowerState = LowerState typeInfo indexMap getIndexForSearch containerInfos end mempty
      moduleStmts =
        [ declareType $ map (\(_, _, m) -> m) containerInfos
        , compileInit
        , compileDestroy
        , compileRun ra
        -- TODO: functions to read/write values,
        -- for each relation? "generateFnsForRelations"
        ]
   in flattenBlocks $ runCodegen lowerState $ traverse emit moduleStmts

-- NOTE: this removes nested Blocks, to simplify the EIR AST
-- TODO: do this in 1 pass together with rest of this module
-- maybe generateProgramInstructions should only use recursion schemes starting in some subtree?
flattenBlocks :: EIR -> EIR
flattenBlocks = cata f
  where
    f = \case
      EIR.BlockF stmts ->
        EIR.Block $ flip concatMap stmts $ \case
          EIR.Block stmts -> stmts
          stmt -> [stmt]
      e -> embed e

compileInit :: CodegenM EIR
compileInit = do
  program <- var "program"
  initActions <- forEachRelation program (\_ci relationPtr -> call EIR.InitializeEmpty [relationPtr])
  fn "eclair_program_init" [] $
    assign program heapAllocProgram
    : initActions
    -- Open question: if some facts are known at compile time, search for derived facts up front?
    ++ [ ret program ]

compileDestroy :: CodegenM EIR
compileDestroy = do
  let program = fnArg 0
  destroyActions <- forEachRelation program (\_ci relationPtr -> call EIR.Destroy [relationPtr])
  fn "eclair_program_destroy" [EIR.Pointer EIR.Program] $
    destroyActions
    ++ [ freeProgram program ]

compileRun :: RA -> CodegenM EIR
compileRun ra = do
  end <- endLabel <$> getLowerState
  fn "eclair_program_run" [EIR.Pointer EIR.Program] $
    generateProgramInstructions ra : [ jump end, label end ]

-- TODO: use emit here (and not on top level) to prevent multiple nested blocks?
generateProgramInstructions :: RA -> CodegenM EIR
generateProgramInstructions = zygo constraintsForSearch $ \case
  RA.ModuleF (map snd -> actions) -> block actions
  RA.ParF (map snd -> actions) -> parallel actions
  RA.ProjectF r (map snd -> unresolvedValues) -> do
    -- NOTE: Value type is the same for all (but not the insert function)
    values <- withProjectState r $ sequence unresolvedValues
    let values' = map pure values  -- TODO refactor, withProjectState can wrap this entire piece of code?
    indices <- indexesForRelation r
    var <- var "value"
    let allocValue = assign var $ stackAlloc EIR.Value r
        assignStmts = zipWith (assign . fieldAccess var) [0..] values'
        insertStmts = flip map indices $ \idx ->
          call EIR.Insert [lookupRelationByIndex r idx, var]
    block $ allocValue : assignStmts ++ insertStmts
  RA.PurgeF r -> do
    actions <- relationUnaryFn r EIR.Purge
    block actions
  RA.MergeF r1 r2 -> do
    actions <- relationBinFn r1 r2 EIR.Merge
    block actions
  RA.SwapF r1 r2 -> do
    actions <- relationBinFn r1 r2 EIR.Swap
    block actions
  RA.LoopF (map snd -> actions) -> do
    end <- labelId "loop.end"
    block [withEndLabel end $ loop actions, label end]
  RA.ExitF rs -> do
    end <- endLabel <$> getLowerState
    -- TODO: foldl' also possible here?
    foldr' f (jump end) =<< traverse getFirstFieldOffset rs
    where
      f field inner =
        let programPtr = fnArg 0
            relationPtr = fieldAccess programPtr field
            isEmpty = call EIR.IsEmpty [relationPtr]
         in if' isEmpty inner
  RA.LitF x -> lit x
  RA.ConstrainF (snd -> lhs) (snd -> rhs) ->
    equals lhs rhs
  RA.NotElemF r (map snd -> columnValues) -> do
    value <- var "value"
    let idx = mkFindIndex columnValues
        relationPtr = lookupRelationByIndex r idx
        allocValue = assign value $ stackAlloc EIR.Value r
    containsVar <- var "contains_result"
    let assignActions = zipWith (assign . fieldAccess value) [0..] columnValues
    block $ allocValue : assignActions
         ++ [ assign containsVar $ call EIR.Contains [relationPtr, value]
            , not' containsVar -- TODO: need emit here
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
        -- TODO: check access, see original code
        fieldAccess (pure value) col
  RA.SearchF r alias clauses (snd -> action) -> do
    (idx, columns) <- idxFromConstraints r (concatMap fst clauses)
    let relationPtr = lookupRelationByIndex r idx
        lbValue = initValue r LowerBound columns
        ubValue = initValue r UpperBound columns
        query = List.foldl1' and' $ map snd clauses
    rangeQuery r relationPtr lbValue ubValue $ \iter -> do
      let currValue = call EIR.IterCurrent [iter]
      --  TODO: use non empty?
      case length clauses of
        0 -> -- No query to check: always matches
          withUpdatedAlias alias currValue action
        _ -> do
          -- TODO: check if this works, probably not..
          value <- currValue
          let isMatch = withSearchState alias value query
          if' isMatch $ withUpdatedAlias alias currValue action

rangeQuery :: Relation
           -> CodegenM EIR
           -> CodegenM EIR
           -> CodegenM EIR
           -> (CodegenM EIR -> CodegenM EIR)
           -> CodegenM EIR
rangeQuery r relationPtr lbValue ubValue loopAction = do
  beginIter <- var "begin_iter"
  endIter <- var "end_iter"
  endLabel <- labelId "range_query.end"
  let allocBeginIter = assign beginIter $ stackAlloc EIR.Iter r
      allocEndIter = assign endIter $ stackAlloc EIR.Iter r
      initLB = call EIR.IterLowerBound [relationPtr, lbValue, beginIter]
      initUB = call EIR.IterUpperBound [relationPtr, ubValue, endIter]
      advanceIter = call EIR.IterNext [beginIter]
      hasNext = not' $ call EIR.IterIsEqual [beginIter, endIter]
      stopIfFinished = if' hasNext (jump endLabel)
      loopStmts = [stopIfFinished, loopAction beginIter, advanceIter]
  block [allocBeginIter, allocEndIter, initLB, initUB, loop loopStmts, label endLabel]

data Bound
  = LowerBound
  | UpperBound

initValue :: Relation -> Bound -> [Column] -> CodegenM EIR
initValue r bound columns = do
  typeInfo <- typeEnv <$> getLowerState
  value <- var "value"
  let allocValue = assign value $ stackAlloc EIR.Value r
      columnNrs = take (length typeInfo) [0..]
      valuesWithCols = [(nr, pure x) | nr <- columnNrs, let x = if nr `elem` columns
                                                                then bounded
                                                                else dontCare]
      -- TODO: take actual values into account:
      assignStmts = map (\(i, val) -> assign (fieldAccess value i) val) valuesWithCols
  -- TODO: need emit
  block $ allocValue : assignStmts ++ [value]
  where
    -- NOTE: only supports unsigned integers for now!
    bounded = EIR.Lit $ case bound of
      LowerBound -> 0
      UpperBound -> 0xffffffff
    dontCare = EIR.Lit 0

forEachRelation :: CodegenM EIR -> (ContainerInfo -> CodegenM EIR -> CodegenM EIR) -> CodegenM [CodegenM EIR]
forEachRelation program f = do
  cis <- containerInfos <$> getLowerState
  pure $ zipWith doCall [0..] cis
  where
    doCall fieldOffset ci =
      f ci (fieldAccess program fieldOffset)

relationUnaryFn :: Relation -> EIR.EIRFunction -> CodegenM [CodegenM EIR]
relationUnaryFn r fn = forEachIndex r $ \idx -> do
  call fn [lookupRelationByIndex r idx]

-- NOTE: assumes r1 and r2 have same underlying representation
-- (guaranteed by earlier compiler stages)
relationBinFn :: Relation -> Relation -> EIR.EIRFunction -> CodegenM [CodegenM EIR]
relationBinFn r1 r2 fn = forEachIndex r1 $ \idx -> do
  call fn [ lookupRelationByIndex r1 idx
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
      let meta = M.mkMeta idx $ fromJust $ Map.lookup r typeInfo
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
