
module Eclair.RA.Lower ( compileToEIR ) where

import Prelude
import Data.Maybe (fromJust)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Eclair.Common.Id
import Eclair.Common.Literal
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
      lowerState = mkLowerState typedefInfo indexMap getIndexForSearch (map fst containerInfos') end
      moduleStmts :: [CodegenM EIR]
      moduleStmts =
        [ declareProgram $ map (\((r, _), m) -> (r, m)) containerInfos'
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
  relationInitActions <- forEachRelation program $ \(r, idx) relationPtr ->
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
  relationDestroyActions <- forEachRelation program $ \(r, idx) relationPtr ->
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
  RA.ModuleF _ (map extract -> actions) -> block actions
  RA.ParF _ (map extract -> actions) -> parallel actions
  RA.SearchF _ r alias clauses (extract -> action) -> do
    let eqsInSearch = foldMap tSnd clauses
        eqs = concatMap normalizedEqToConstraints eqsInSearch
    idx <- idxFromConstraints r alias eqs
    let relationPtr = lookupRelationByIndex r idx
        isConstrain = \case
          RA.CompareOp {} -> True
          _ -> False
        queryClauses = map extract $ filter ((not . isConstrain) . tFst) clauses
        query = List.foldl1' and' queryClauses
    (initBoundValues, lbValue, ubValue) <- initValue r idx alias eqsInSearch
    block
      [ initBoundValues
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
  RA.ProjectF _ r (map extract -> unresolvedValues) -> do
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
  RA.PurgeF _ r ->
    block =<< relationUnaryFn r EIR.Purge
  RA.SwapF _ r1 r2 ->
    block =<< relationBinFn r1 r2 EIR.Swap
  RA.MergeF _ r1 r2 -> do
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
  RA.LoopF _ (map extract -> actions) -> do
    end <- labelId "loop.end"
    block [withEndLabel end $ loop actions, label end]
  RA.IfF _ (extract -> condition) (extract -> action) -> do
    if' condition action
  RA.PrimOpF _ (RA.BuiltinOp op) (map extract -> args) ->
    case args of
      [lhs, rhs] -> do
        let toArithmetic = case op of
              RA.Plus -> plus
              RA.Minus -> minus
              RA.Multiply -> multiply
              RA.Divide -> divide
        toArithmetic lhs rhs
      _ ->
        panic "Unexpected case in 'generateProgramInstructions' while lowering RA!"
  RA.PrimOpF _ (RA.ExternOp opName) (map extract -> args) -> do
    mkExternOp opName args
  RA.CompareOpF _ op (extract -> lhs) (extract -> rhs) -> do
    let toComparison = case op of
          RA.Equals -> equals
          RA.NotEquals -> notEquals
          RA.LessThan -> lessThan
          RA.LessOrEqual -> lessOrEqual
          RA.GreaterThan -> greaterThan
          RA.GreaterOrEqual -> greaterOrEqual
    toComparison lhs rhs
  RA.ExitF _ rs -> do
    end <- endLabel <$> getLowerState
    foldl' f (jump end) =<< traverse getFirstFieldOffset rs
    where
      f inner field = do
        (r, idx) <- getContainerInfoByOffset field
        let programPtr = fnArg 0
            relationPtr = fieldAccess programPtr field
            isEmpty = call r idx EIR.IsEmpty [relationPtr]
        if' isEmpty inner
  RA.LitF _ x -> lit x
  RA.NotElemF _ r values -> do
    if containsUndefined $ map tFst values
      then existenceCheckPartialSearch
      else existenceCheckTotalSearch
    where
      containsUndefined = isJust . find (\case
        RA.Undef {} -> True
        _ -> False)
      existenceCheckPartialSearch = do
        getIndexForSearch <- idxSelector <$> getLowerState
        lbValue <- var "lower_bound_value"
        ubValue <- var "upper_bound_value"
        beginIter <- var "begin_iter"
        endIter <- var "end_iter"
        isEmpty <- var "is_empty"
        let values' = map tFst values
            cs = definedColumnsFor values'
            signature = SearchSignature $ Set.fromList cs
            idx = getIndexForSearch r signature
            lbValueWithCols = zipWith (curry $ second $ lowerConstrainValue (lit 0x00000000)) [0..] values'
            ubValueWithCols = zipWith (curry $ second $ lowerConstrainValue (lit 0xffffffff)) [0..] values'
            lbAssignStmts = block $ map (\(i, val) -> assign (fieldAccess lbValue i) val) lbValueWithCols
            ubAssignStmts = block $ map (\(i, val) -> assign (fieldAccess ubValue i) val) ubValueWithCols
            relationPtr = lookupRelationByIndex r idx
        block
          [ assign lbValue $ stackAlloc r idx EIR.Value
          , lbAssignStmts
          , assign ubValue $ stackAlloc r idx EIR.Value
          , ubAssignStmts
          , assign beginIter $ stackAlloc r idx EIR.Iter
          , assign endIter $ stackAlloc r idx EIR.Iter
          , call r idx EIR.IterLowerBound [relationPtr, lbValue, beginIter]
          , call r idx EIR.IterUpperBound [relationPtr, ubValue, endIter]
          , assign isEmpty $ call r idx EIR.IterIsEqual [beginIter, endIter]
          , not' isEmpty
          ]
      existenceCheckTotalSearch = do
        let columnValues = map extract values
        value <- var "value"
        let idx = mkFindIndex columnValues
            relationPtr = lookupRelationByIndex r idx
            allocValue = assign value $ stackAlloc r idx EIR.Value
        containsVar <- var "contains_result"
        let assignActions = zipWith (assign . fieldAccess value) [0..] columnValues
            containsAction = assign containsVar $ call r idx EIR.Contains [relationPtr, value]
        block $ allocValue : assignActions <> [containsAction, not' containsVar]
  RA.ColumnIndexF _ a' col -> ask >>= \case
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
  RA.UndefF {} ->
    panic "Undef should not occur when lowering to EIR!"
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

-- NOTE: only supports unsigned integers for now!
initValue :: Relation -> Index -> RA.Alias -> [NormalizedEquality]
          -> CodegenM (CodegenM EIR, CodegenM EIR, CodegenM EIR)
initValue r idx a eqs = do
  let r' = stripIdPrefixes r
  typeInfo <- fromJust . Map.lookup r' . typeEnv <$> getLowerState
  lbValue <- var "lower_bound_value"
  ubValue <- var "upper_bound_value"
  let columnNrs = take (length typeInfo) [0..]
      lbAllocValue = assign lbValue $ stackAlloc r idx EIR.Value
      ubAllocValue = assign ubValue $ stackAlloc r idx EIR.Value
      -- TODO stack allocate all values so they are not computed twice?
      lbValuesWithCols = [(nr, x) | nr <- columnNrs, let x = constrain (lit 0x00000000) nr]
      ubValuesWithCols = [(nr, x) | nr <- columnNrs, let x = constrain (lit 0xffffffff) nr]
      lbAssignStmts = map (\(i, val) -> assign (fieldAccess lbValue i) val) lbValuesWithCols
      ubAssignStmts = map (\(i, val) -> assign (fieldAccess ubValue i) val) ubValuesWithCols
  pure (block $ lbAllocValue : ubAllocValue : lbAssignStmts <> ubAssignStmts, lbValue, ubValue)
  where
    constrain bound col =
      case find (\(NormalizedEquality a' col' _) -> a == a' && col == col') eqs of
        Nothing ->
          bound
        Just (NormalizedEquality _ _ ra) ->
          lowerConstrainValue bound ra
      -- let NormalizedEquality _ _ ra = fromJust $ find (\(NormalizedEquality a' col' _) -> a == a' && col == col') eqs
      --  in lowerConstrainValue bound ra

lowerConstrainValue :: CodegenM EIR -> RA -> CodegenM EIR
lowerConstrainValue bound = \case
  RA.Lit _ x ->
    lit x
  RA.Undef _ ->
    bound
  RA.ColumnIndex _ a' col' ->
    fieldAccess (lookupAlias a') col'
  RA.PrimOp _ (RA.BuiltinOp op) [lhs, rhs] ->
    mkArithOp op (lowerConstrainValue bound lhs) (lowerConstrainValue bound rhs)
  RA.PrimOp _ (RA.ExternOp opName) args ->
    mkExternOp opName $ map (lowerConstrainValue bound) args
  _ -> panic "Unsupported initial value while lowering to RA"

forEachRelation :: CodegenM EIR -> ((Relation, Index) -> CodegenM EIR -> CodegenM EIR) -> CodegenM [CodegenM EIR]
forEachRelation program f = do
  cis <- relInfo . cgInfo <$> getLowerState
  pure $ zipWith doCall [1..] cis
  where
    doCall fieldOffset relationInfo =
      f relationInfo (fieldAccess program fieldOffset)

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

getContainerInfos :: IndexMap -> TypedefInfo -> [((Relation, Index), M.Metadata)]
getContainerInfos indexMap typedefInfo = containerInfos'
  where
    combinations r idxs =
      (r,) <$> Set.toList idxs
    toContainerInfo r idx =
      let r' = stripIdPrefixes r
          meta = M.mkMeta idx $ fromJust $ Map.lookup r' typedefInfo
       in ((r, idx), meta)
    storesList = Map.foldMapWithKey combinations indexMap
    containerInfos' = map (uncurry toContainerInfo) storesList

-- NOTE: only use this index for a total search (all columns constrained)
mkFindIndex :: [a] -> Index
mkFindIndex =
  Index . zipWith const [0..]

indexesForRelation :: Relation -> CodegenM [Index]
indexesForRelation r =
  Set.toList . fromJust . Map.lookup r . idxMap <$> getLowerState
