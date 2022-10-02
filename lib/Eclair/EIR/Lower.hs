{-# LANGUAGE RecursiveDo #-}

module Eclair.EIR.Lower
  ( compileToLLVM
  ) where

import Prelude hiding (void)
import Control.Monad.Morph hiding (embed)
import Data.Traversable (for)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.List as L
import Data.List ((!!))
import Data.Maybe (fromJust)
import LLVM.Codegen as LLVM
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.LLVM.BTree as BTree
import qualified Eclair.LLVM.Symbol as Symbol
import qualified Eclair.LLVM.Vector as Vector
import qualified Eclair.LLVM.HashMap as HashMap
import qualified Eclair.LLVM.SymbolTable as SymbolTable
import Eclair.EIR.Codegen
import Eclair.LLVM.LLVM
import Eclair.LLVM.Metadata
import Eclair.LLVM.Hash
import Eclair.LLVM.Runtime
import Eclair.RA.IndexSelection
import Eclair.Comonads
import Eclair.AST.IR
import Eclair.Id


type EIR = EIR.EIR
type EIRF = EIR.EIRF
type Relation = EIR.Relation

compileToLLVM :: EIR -> IO Module
compileToLLVM = \case
  EIR.Block (EIR.DeclareProgram metas : decls) -> runModuleBuilderT $ do
    exts <- createExternals
    (metaMapping, fnss) <- runCacheT $ traverse (codegenRuntime exts . snd) metas
    codegenDebugInfos metaMapping
    (symbolTable, symbol) <- codegenSymbolTable exts
    let symbolTableTy = SymbolTable.tySymbolTable symbolTable
        fnsInfo = zip (map (map getIndexFromMeta) metas) fnss
        fnsMap = M.fromList fnsInfo
    -- TODO: add hash based on filepath of the file we're compiling?
    programTy <- typedef "program" Off (symbolTableTy : map typeObj fnss)
    programSize <- withLLVMTypeInfo $ \ctx td -> llvmSizeOf ctx td programTy
    let lowerState = LowerState programTy programSize symbolTable symbol fnsMap mempty exts
    traverse_ (processDecl lowerState) decls
    usingReaderT (metas, lowerState) $ do
      addFactsFn <- generateAddFactsFn
      generateAddFact addFactsFn
      generateGetFactsFn
      generateFreeBufferFn
      generateFactCountFn
      generateEncodeStringFn
      generateDecodeStringFn
  _ ->
    panic "Unexpected top level EIR declarations when compiling to LLVM!"
  where
    processDecl lowerState = \case
      EIR.Function name tys retTy body -> do
        let unusedRelation = panic "Unexpected use of relation for function type when lowering EIR to LLVM."
            unusedIndex = panic "Unexpected use of index for function type when lowering EIR to LLVM."
            getType ty = evalStateT (toLLVMType unusedRelation unusedIndex ty) lowerState
        argTypes <- liftIO $ traverse getType tys
        returnType <- liftIO $ getType retTy
        let args = map (, ParameterName "arg") argTypes
        function (Name name) args returnType $ \args -> do
          runCodegenM (fnBodyToLLVM args body) lowerState
      _ ->
        panic "Unexpected top level EIR declaration when compiling to LLVM!"

fnBodyToLLVM :: [Operand] -> EIR -> CodegenM ()
fnBodyToLLVM args = lowerM instrToOperand instrToUnit
  where
    instrToOperand :: EIRF (EIR, CodegenM Operand) -> CodegenM Operand
    instrToOperand = \case
      EIR.FunctionArgF pos ->
        pure $ args !! pos
      EIR.FieldAccessF (snd -> structOrVar) pos -> do
        -- NOTE: structOrVar is always a pointer to a heap-/stack-allocated
        -- value so we need to first deref the pointer, and then index into the
        -- fields of the value ('addr' does this for us). On top of that, we
        -- can only compute the address here and not do a load as well, since
        -- sometimes this pointer is used in a "store" instruction.
        addr (mkPath [int32 $ toInteger pos]) =<< structOrVar
      EIR.VarF v ->
        lookupVar v
      EIR.NotF (snd -> bool) ->
        not' =<< bool
      EIR.AndF (snd -> bool1) (snd -> bool2) -> do
        b1 <- bool1
        b2 <- bool2
        and b1 b2
      EIR.EqualsF (a, lhs) (b, rhs) -> do
        valueA <- loadIfNeeded lhs a
        valueB <- loadIfNeeded rhs b
        valueA `eq` valueB
      EIR.CallF r idx fn (map snd -> args) ->
        doCall r idx fn args
      EIR.PrimOpF op (map snd -> args) ->
        doPrimOp op args
      EIR.HeapAllocateProgramF -> do
        (malloc, (programTy, programSize)) <- gets (extMalloc . externals &&& programType &&& programSizeBytes)
        let memorySize = int32 $ fromIntegral programSize
        pointer <- call malloc [memorySize] `named` "memory"
        pointer `bitcast` ptr programTy
      EIR.StackAllocateF r idx ty -> do
        theType <- toLLVMType r idx ty
        alloca theType (Just (int32 1)) 0
      EIR.LitF (LNumber value) ->
        pure $ int32 (fromIntegral value)
      EIR.LitF (LString value) ->
        -- TODO: unique name?
        globalUtf8StringPtr value "string_literal"
      _ ->
        panic "Unhandled pattern match case in 'instrToOperand' while lowering EIR to LLVM!"
    instrToUnit :: (EIRF (Triple EIR (CodegenM Operand) (CodegenM ())) -> CodegenM ())
    instrToUnit = \case
      EIR.BlockF stmts -> do
        traverse_ toInstrs stmts
      EIR.ParF stmts ->
        -- NOTE: this is just sequential evaluation for now
        traverse_ toInstrs stmts
      EIR.AssignF (toOperandWithContext -> (operand, eirLHS))
                  (toOperandWithContext -> (val, eirRHS)) -> do
        case eirLHS of
          EIR.Var varName -> do
            -- Assigning to a variable: evaluate the value, and add to the varMap.
            -- This allows for future lookups of a variable.
            value <- val `named` Name varName
            addVarBinding varName value
          _ -> do
            -- NOTE: here we assume we are assigning to an operand (of a struct field)
            -- "operand" will contain a pointer, "val" will contain the actual value
            -- We need to store the result to the address the pointer is pointing to.
            address <- operand
            value <- loadIfNeeded val eirRHS
            store address 0 value
      EIR.FreeProgramF (toOperand -> programVar) -> do
        freeFn <- gets (extFree . externals)
        program <- programVar
        memory <- program `bitcast` ptr i8 `named` "memory"
        () <$ call freeFn [memory]
      EIR.CallF r idx fn (map toOperand -> args) ->
        () <$ doCall r idx fn args
      EIR.PrimOpF op (map toOperand -> args) ->
        () <$ doPrimOp op args
      EIR.LoopF stmts ->
        loop $ traverse_ toInstrs stmts
      EIR.IfF (toOperand -> cond) (toInstrs -> body) -> do
        condition <- cond
        if' condition body
      EIR.JumpF lbl ->
        br (labelToName lbl)
      EIR.LabelF lbl ->
        -- NOTE: the label should be globally unique thanks to the RA -> EIR lowering pass
        emitBlockStart $ labelToName lbl
      EIR.ReturnF (toOperand -> value) ->
        ret =<< value
      _ ->
        panic "Unhandled pattern match case in 'instrToUnit' while lowering EIR to LLVM!"
    toOperand (Triple _ operand _) = operand
    toOperandWithContext (Triple eir operand _) =
      (operand, eir)
    toInstrs (Triple _ _ instrs) = instrs
    doCall :: Relation -> Index -> EIR.Function -> [CodegenM Operand] -> CodegenM Operand
    doCall r idx fn args = do
      argOperands <- sequence args
      func <- lookupFunction r idx fn
      call func argOperands
    doPrimOp :: EIR.Op -> [CodegenM Operand] -> CodegenM Operand
    doPrimOp op args = do
      argOperands <- sequence args
      fn <- lookupPrimOp op
      call fn argOperands

-- Here be recursion-schemes dragons...
--
-- lowerM is a recursion-scheme that behaves like a zygomorphism, but it is
-- enhanced in the sense that both functions passed to the zygomorphism have
-- access to the original subtree.
--
-- NOTE: zygo effect is kind of abused here, since due to lazyness we can choose what
-- we need to compile to LLVM: instructions either return "()" or an "Operand".
-- para effect is needed since we need access to the original subtree in the
-- assignment case to check if we are assigning to a variable or not, allowing
-- us to easily transform an "expression-oriented" EIR to statement based LLVM IR.
lowerM :: (EIRF (EIR, CodegenM Operand) -> CodegenM Operand)
       -> (EIRF (Triple EIR (CodegenM Operand) (CodegenM ())) -> CodegenM ())
       -> EIR
       -> CodegenM ()
lowerM f = gcata (distribute f)
  where
    distribute
      :: Corecursive t
      => (Base t (t, b) -> b)
      -> (Base t (Triple t b a) -> Triple t b (Base t a))
    distribute g m =
      let base_t_t = map tFst m
          base_t_tb = map (tFst &&& tSnd) m
          base_t_a = map tThd m
       in Triple (embed base_t_t) (g base_t_tb) base_t_a

type CacheT = StateT (Map Metadata (Suffix, Functions))

runCacheT :: Monad m => CacheT m a -> m (Map Metadata Suffix, a)
runCacheT m = do
  (a, s) <- runStateT m mempty
  pure (map fst s, a)

codegenRuntime :: Externals -> Metadata -> CacheT (ModuleBuilderT IO) Functions
codegenRuntime exts meta = gets (M.lookup meta) >>= \case
  Nothing -> do
    suffix <- gets length
    fns <- cgRuntime suffix
    modify $ M.insert meta (suffix, fns)
    pure fns
  Just (_, cachedFns) -> pure cachedFns
  where
    cgRuntime suffix = lift $ case meta of
      BTree meta -> BTree.codegen suffix exts meta

codegenDebugInfos :: Monad m => Map Metadata Int -> ModuleBuilderT m ()
codegenDebugInfos metaMapping =
  traverse_ (uncurry codegenDebugInfo) $ M.toList metaMapping
  where
    codegenDebugInfo meta i =
      let hash = getHash meta
          name = Name $ ("specialize_debug_info." <>) $ unHash hash
       in global name i32 (Int 32 $ toInteger i)

codegenSymbolTable :: Externals -> ModuleBuilderT IO (SymbolTable.SymbolTable, Symbol.Symbol)
codegenSymbolTable exts = do
  symbol <- hoist intoIO $ Symbol.codegen exts

  let tySymbol = Symbol.tySymbol symbol
      freeFn = extFree exts
      symbolDestructor iterPtr = do
        call (Symbol.symbolDestroy symbol) [iterPtr]
        pass

  -- Only this vector does the cleanup of all the symbols, to prevent double frees
  vec <- Vector.codegen tySymbol exts (Just symbolDestructor)
  hashMap <- HashMap.codegen symbol exts
  symbolTable <- hoist intoIO $ SymbolTable.codegen tySymbol vec hashMap
  pure (symbolTable, symbol)
  where
    intoIO = pure . runIdentity

-- TODO: add hash based on filepath of the file we're compiling?
mkType :: Name -> [Functions] -> ModuleBuilderT IO LLVM.Type
mkType name fnss =
  typedef name Off tys
  where
    tys = map typeObj fnss

getIndexFromMeta :: Metadata -> Index
getIndexFromMeta = \case
  BTree meta -> Index $ BTree.index meta

createExternals :: ModuleBuilderT IO Externals
createExternals = do
  mallocFn <- extern "malloc" [i32] (ptr i8)
  freeFn <- extern "free" [ptr i8] void
  memsetFn <- extern "llvm.memset.p0i8.i64" [ptr i8, i8, i64, i1] void
  memcpyFn <- extern "llvm.memcpy.p0i8.p0i8.i64" [ptr i8, ptr i8, i64, i1] void
  memcmpFn <- extern "memcmp" [ptr i8, ptr i8, i64] i32
  pure $ Externals mallocFn freeFn memsetFn memcpyFn memcmpFn


generateAddFact :: MonadFix m => Operand -> CodegenInOutT (ModuleBuilderT m) Operand
generateAddFact addFactsFn = do
  lowerState <- asks snd
  let args = [ (ptr (programType lowerState), ParameterName "eclair_program")
             , (i16, ParameterName "fact_type")
             , (ptr i32, ParameterName "memory")
             ]
      returnType = void

  function "eclair_add_fact" args returnType $ \[program, factType, memory] -> do
    call addFactsFn [program, factType, memory, int32 1]
    retVoid

generateAddFactsFn :: MonadFix m => CodegenInOutT (ModuleBuilderT m) Operand
generateAddFactsFn = do
  (metas, lowerState) <- ask
  let relations = getRelations metas
      mapping = getFactTypeMapping metas

  -- NOTE: this assumes there are no more than 65536 facts in a single Datalog program
  let args = [ (ptr (programType lowerState), ParameterName "eclair_program")
             , (i16, ParameterName "fact_type")
             , (ptr i32, ParameterName "memory")
             , (i32, ParameterName "fact_count")
             ]
      returnType = void
  function "eclair_add_facts" args returnType $ \[program, factType, memory, factCount] -> do
    switchOnFactType metas retVoid factType $ \r -> do
      let indexes = indexesFor lowerState r
      for_ indexes $ \idx -> do
        let numCols = fromIntegral $ factNumColumns r metas
            treeOffset = int32 $ toInteger $ getContainerOffset metas r idx
        relationPtr <- gep program [int32 0, treeOffset]
        -- TODO: don't re-calculate this type, do this based on value datatype created in each runtime data structure
        arrayPtr <- memory `bitcast` ptr (ArrayType numCols i32)

        loopFor (int32 0) (`ult` factCount) (add (int32 1)) $ \i -> do
          valuePtr <- gep arrayPtr [i]
          call (lsLookupFunction r idx EIR.Insert lowerState) [relationPtr, valuePtr]

generateGetFactsFn :: MonadFix m => CodegenInOutT (ModuleBuilderT m) Operand
generateGetFactsFn = do
  (metas, lowerState) <- ask
  let args = [ (ptr (programType lowerState), ParameterName "eclair_program")
             , (i16, ParameterName "fact_type")
             ]
      returnType = ptr i32
      mallocFn = extMalloc $ externals lowerState
  function "eclair_get_facts" args returnType $ \[program, factType] -> do
    switchOnFactType metas (ret $ nullPtr i32) factType $ \r -> do
      let indexes = indexesFor lowerState r
          idx = fromJust $ viaNonEmpty head indexes  -- TODO: which idx? just select first matching?
          lookupFn fn = lsLookupFunction r idx fn lowerState
          doCall fn = call (lookupFn fn)
          numCols = factNumColumns r metas
          valueSize = 4 * numCols  -- TODO: should use LLVM "valueSize" instead of re-calculating here
          treeOffset = int32 $ toInteger $ getContainerOffset metas r idx
      relationPtr <- gep program [int32 0, treeOffset]
      relationSize <- (doCall EIR.Size [relationPtr] >>= (`trunc` i32)) `named` "fact_count"
      memorySize <- mul relationSize (int32 $ toInteger valueSize) `named` "byte_count"
      memory <- call mallocFn [memorySize] `named` "memory"
      arrayPtr <- memory `bitcast` ptr (ArrayType (fromIntegral numCols) i32) `named` "array"

      iPtr <- alloca i32 (Just (int32 1)) 0 `named` "i"
      store iPtr 0 (int32 0)
      let iterTy = evalState (toLLVMType r idx EIR.Iter) lowerState
      currIter <- alloca iterTy (Just (int32 1)) 0 `named` "current_iter"
      endIter <- alloca iterTy (Just (int32 1)) 0 `named` "end_iter"
      doCall EIR.IterBegin [relationPtr, currIter]
      doCall EIR.IterEnd [relationPtr, endIter]
      let loopCondition = do
            isEqual <- doCall EIR.IterIsEqual [currIter, endIter]
            not' isEqual
      loopWhile loopCondition $ do
        i <- load iPtr 0
        valuePtr <- gep arrayPtr [i] `named` "value"
        currentVal <- doCall EIR.IterCurrent [currIter] `named` "current"
        copy (mkPath []) currentVal valuePtr
        i' <- add i (int32 1)
        store iPtr 0 i'
        doCall EIR.IterNext [currIter]

      ret =<< memory `bitcast` ptr i32

generateFreeBufferFn :: Monad m => CodegenInOutT (ModuleBuilderT m) Operand
generateFreeBufferFn = do
  lowerState <- asks snd
  let freeFn = extFree $ externals lowerState
      args = [(ptr i32, ParameterName "buffer")]
      returnType = void
  function "eclair_free_buffer" args returnType $ \[buf] -> mdo
    memory <- buf `bitcast` ptr i8 `named` "memory"
    call freeFn [memory]
    retVoid

generateFactCountFn :: MonadFix m => CodegenInOutT (ModuleBuilderT m) Operand
generateFactCountFn = do
  (metas, lowerState) <- ask
  let args = [ (ptr (programType lowerState), ParameterName "eclair_program")
             , (i16, ParameterName "fact_type")
             ]
      returnType = i64
  function "eclair_fact_count" args returnType $ \[program, factType] -> do
    switchOnFactType metas (ret $ int64 0) factType $ \r -> do
      let indexes = indexesFor lowerState r
          idx = fromJust $ viaNonEmpty head indexes  -- TODO: which idx? just select first matching?
          lookupFn fn = lsLookupFunction r idx fn lowerState
          doCall fn = call (lookupFn fn)
          treeOffset = int32 $ toInteger $ getContainerOffset metas r idx
      relationPtr <- gep program [int32 0, treeOffset]
      relationSize <- doCall EIR.Size [relationPtr]
      ret relationSize

-- NOTE: string does not need to be 0-terminated, length field is used to determine length (in bytes).
-- Eclair makes an internal copy of the string, for simpler memory management.
generateEncodeStringFn :: MonadFix m => CodegenInOutT (ModuleBuilderT m) Operand
generateEncodeStringFn = do
  lowerState <- asks snd
  let args = [ (ptr (programType lowerState), "eclair_program")
             , (i32, "string_length")
             , (ptr i8, "string_data")
             ]
      (symbolTable, symbol) = (symbolTableFns &&& symbolFns) lowerState
      exts = externals lowerState

  function "eclair_encode_string" args i32 $ \[program, len, stringData] -> do
    lenBytes <- zext len i64
    stringDataCopy <- call (extMalloc exts) [lenBytes]
    call (extMemcpy exts) [stringDataCopy, stringData, lenBytes, bit 0]

    symbolPtr <- alloca (Symbol.tySymbol symbol) (Just (int32 1)) 0
    call (Symbol.symbolInit symbol) [symbolPtr, len, stringDataCopy]

    symbolTablePtr <- getSymbolTablePtr program
    index <- call (SymbolTable.symbolTableLookupIndex symbolTable) [symbolTablePtr, symbolPtr]
    alreadyContainsSymbol <- index `ne` int32 0xFFFFFFFF
    if' alreadyContainsSymbol $ do
      -- Since the string was not added to the table, the memory pointed to by
      -- the symbol is not managed by the symbol table, so we need to manually free the data.
      call (extFree exts) [stringDataCopy]
      ret index

    -- No free needed here, automatically called when symbol table is cleaned up.
    ret =<< call (SymbolTable.symbolTableFindOrInsert symbolTable) [symbolTablePtr, symbolPtr]

-- NOTE: do not free the returned string/byte array,
-- this happens automatically when eclair_destroy is called
generateDecodeStringFn :: MonadFix m => CodegenInOutT (ModuleBuilderT m) Operand
generateDecodeStringFn = do
  lowerState <- asks snd
  let args = [ (ptr (programType lowerState), "eclair_program")
             , (i32, "string_index")
             ]
      retTy = ptr i8  -- actually a pointer to a symbol (possibly null if not found),
                      -- some extra decoding needs to happen on side of host language.
      symbolTable = symbolTableFns lowerState

  function "eclair_decode_string" args (ptr i8) $ \[program, idx] -> do
    symbolTablePtr <- getSymbolTablePtr program
    containsIndex <- call (SymbolTable.symbolTableContainsIndex symbolTable) [symbolTablePtr, idx]
    if' containsIndex $ do
      symbolPtr <- call (SymbolTable.symbolTableLookupSymbol symbolTable) [symbolTablePtr, idx]
      ret =<< symbolPtr `bitcast` ptr i8

    ret $ nullPtr i8

-- TODO: move all lower level code below to Codegen.hs to keep high level overview here!

getSymbolTablePtr :: (MonadNameSupply m, MonadModuleBuilder m, MonadIRBuilder m)
                  => Operand -> m Operand
getSymbolTablePtr program =
  gep program [int32 0]

type InOutState = ([(Relation, Metadata)], LowerState)

type CodegenInOutT = ReaderT  InOutState

switchOnFactType :: MonadFix m
                 => [(Relation, metadata)]
                 -> IRBuilderT m ()
                 -> Operand
                 -> (Relation -> IRBuilderT m ())
                 -> IRBuilderT m ()
switchOnFactType metas defaultCase factType generateCase = mdo
    switch factType end caseBlocks
    caseBlocks <- for mapping $ \(r, factNum) -> do
      caseBlock <- block `named` Name (unId r)
      generateCase r
      pure (int16 factNum, caseBlock)

    end <- block `named` "switch.default"
    defaultCase
  where
    mapping = getFactTypeMapping metas

factNumColumns :: Relation -> [(Relation, Metadata)] -> Int
factNumColumns r metas =
  getNumColumns (snd $ fromJust $ L.find ((== r) . fst) metas)

-- NOTE: disregards all "special" relations, since they should not be visible to the end user!
getRelations :: [(Relation, metadata)] -> [Relation]
getRelations metas =
  ordNub $ filter (not . startsWithIdPrefix) $ map fst metas

getFactTypeMapping :: [(Relation, metadata)] -> [(Relation, Integer)]
getFactTypeMapping metas =
  zip (getRelations metas) [0..]

-- (+1) due to symbol table at position 0 in program struct
getContainerOffset :: [(Relation, Metadata)] -> Relation -> Index -> Int
getContainerOffset metas r idx =
  (+1) . fromJust $ L.findIndex (sameRelationAndIndex r idx) $ map (map getIndex) metas

sameRelationAndIndex :: Relation -> Index -> (Relation, Index) -> Bool
sameRelationAndIndex r idx (r', idx') =
  r == r' && idx == idx'

indexesFor :: LowerState -> Relation -> [Index]
indexesFor ls r =
  map snd $ filter ((== r) . fst) $ M.keys $ fnsMap ls

