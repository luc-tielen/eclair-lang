{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.EIR.Lower
  ( compileToLLVM
  ) where

import Prelude hiding (void)
import qualified Prelude
import qualified Relude (swap)
import Control.Monad.Morph hiding (embed)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.List ((!!))
import Foreign.ForeignPtr
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.LLVM.BTree as BTree
import qualified Eclair.LLVM.Symbol as Symbol
import qualified Eclair.LLVM.Vector as Vector
import qualified Eclair.LLVM.HashMap as HashMap
import qualified Eclair.LLVM.SymbolTable as SymbolTable
import Eclair.EIR.Lower.Codegen
import Eclair.EIR.Lower.Externals
import Eclair.EIR.Lower.API
import Eclair.LLVM.Codegen as LLVM
import qualified LLVM.C.API as LibLLVM
import Eclair.LLVM.Metadata
import Eclair.LLVM.Hash
import Eclair.RA.IndexSelection
import Eclair.Common.Config
import Eclair.Comonads
import Eclair.AST.IR
import Eclair.AST.Transforms.ReplaceStrings (StringMap)
import Eclair.Common.Id
import Eclair.Common.Extern


type EIR = EIR.EIR
type EIRF = EIR.EIRF
type Relation = EIR.Relation

compileToLLVM :: Maybe Target -> StringMap -> Map Relation UsageMode -> [Extern] -> EIR -> IO Module
compileToLLVM target stringMapping usageMapping externDefs eir = do
  ctx <- LibLLVM.mkContext
  llvmMod <- LibLLVM.mkModule ctx "eclair"
  if target == Just Wasm32
    then do
      -- layout string found in Rust compiler (wasm32_unknown_unknown.rs)
      let wasmDataLayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-n32:64-S128-ni:1:10:20"
      td <- LibLLVM.mkTargetData wasmDataLayout
      LibLLVM.setTargetData llvmMod td
      withForeignPtr td $ \tdPtr -> do
        compile (Config target ctx tdPtr) eir
    else do
      -- use host layout
      td <- LibLLVM.getTargetData llvmMod
      compile (Config target ctx td) eir
  where
    compile cfg = \case
      EIR.Module (EIR.DeclareProgram metas : decls) -> runModuleBuilderT $ runConfigT cfg $ do
        let ctx = cfgLLVMContext cfg
            td = cfgTargetData cfg
        exts <- createExternals
        (metaMapping, fnss) <- runCacheT $ traverse (codegenRuntime exts . snd) metas
        codegenDebugInfos metaMapping
        (symbolTable, symbol) <- codegenSymbolTable exts
        let symbolTableTy = SymbolTable.tySymbolTable symbolTable
            fnsInfo = zip (map (map getIndexFromMeta) metas) fnss
            fnsMap' = M.fromList fnsInfo
        -- TODO: add hash based on filepath of the file we're compiling?
        programTy <- typedef "program" Off (symbolTableTy : map typeObj fnss)
        programSize <- withLLVMTypeInfo ctx $ llvmSizeOf ctx td programTy
        lift $ do
          externs <- traverse (processExtern symbolTableTy) externDefs
          let externMap = M.fromList externs
              lowerState = LowerState programTy programSize symbolTable symbol fnsMap' mempty 0 exts externMap
          traverse_ (processDecl lowerState) decls
          codegenAPI relMapping usageMapping metas lowerState
      _ ->
        panic "Unexpected top level EIR declarations when compiling to LLVM!"

    relMapping =
      M.mapKeys Id stringMapping

    processExtern symbolTableTy (Extern fnName argCount extKind) = do
      let argTys = ptr symbolTableTy : replicate argCount i32
          retTy = if extKind == ExternConstraint then i1 else i32  -- i1, or i8 for bool?
      fn <- extern (Name $ unId fnName) argTys retTy
      pure (fnName, fn)

    processDecl lowerState = \case
      EIR.Function visibility name tys retTy body -> do
        let unusedRelation = panic "Unexpected use of relation for function type when lowering EIR to LLVM."
            unusedIndex = panic "Unexpected use of index for function type when lowering EIR to LLVM."
            getType ty = evalStateT (toLLVMType unusedRelation unusedIndex ty) lowerState
        argTypes <- liftIO $ traverse getType tys
        returnType <- liftIO $ getType retTy
        let args = map (, ParameterName "arg") argTypes
            fn = if visibility == EIR.Public then apiFunction else function
        -- Only public functions are exposed, rest is only used internally
        fn (Name name) args returnType $ \args' -> do
          runCodegenM (fnBodyToLLVM args' body) lowerState
      _ ->
        panic "Unexpected top level EIR declaration when compiling to LLVM!"

fnBodyToLLVM :: MonadFix m => [Operand] -> EIR -> CodegenT m ()
fnBodyToLLVM args = lowerM instrToOperand instrToUnit
  where
    instrToOperand :: Monad m => EIRF (EIR, CodegenT m Operand) -> CodegenT m Operand
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
      EIR.NotF (snd -> bool') ->
        not' =<< bool'
      EIR.AndF (snd -> bool1) (snd -> bool2) -> do
        b1 <- bool1
        b2 <- bool2
        and b1 b2
      EIR.PrimOpF op args' ->
        invokePrimOp op args'
      EIR.HeapAllocateProgramF -> do
        (malloc, (programTy, programSize)) <- gets (extMalloc . externals &&& programType &&& programSizeBytes)
        let memorySize = int32 $ fromIntegral programSize
        pointer <- call malloc [memorySize]
        pure $ ptrcast programTy pointer
      EIR.StackAllocateF r idx ty -> do
        theType <- toLLVMType r idx ty
        alloca theType (Just (int32 1)) 0
      EIR.LitF (LNumber value) ->
        pure $ int32 (fromIntegral value)
      EIR.LitF (LString value) -> do
        -- We create a global variable to statically store the string,
        -- but we malloc and copy the string over since the symbol table
        -- frees all symbols at the end.
        varName <- newGlobalVarName "string_literal"
        globalStringPtr <- globalUtf8StringPtr value varName
        let utf8Length = int32 $ toInteger $ BS.length $ encodeUtf8 value
        numBytes <- utf8Length `zext` i64
        exts <- gets externals
        stringPtr <- call (extMalloc exts) [utf8Length]
        _ <- call (extMemcpy exts) [stringPtr, globalStringPtr, numBytes, bit 0]
        symFns <- gets symbolFns
        let tySymbol = Symbol.tySymbol symFns
        symbolPtr <- alloca tySymbol (Just (int32 1)) 0
        _ <- call (Symbol.symbolInit symFns) [symbolPtr, utf8Length, stringPtr]
        pure symbolPtr
      _ ->
        panic "Unhandled pattern match case in 'instrToOperand' while lowering EIR to LLVM!"

    instrToUnit :: MonadFix m => (EIRF (Triple EIR (CodegenT m Operand) (CodegenT m ())) -> CodegenT m ())
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
            value <- val
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
        let memory = ptrcast i8 program
        Prelude.void $ call freeFn [memory]
      EIR.PrimOpF op (map (Relude.swap . toOperandWithContext) -> args') ->
        Prelude.void $ invokePrimOp op args'
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
    invokePrimOp :: Monad m => EIR.Op -> [(EIR, CodegenT m Operand)] -> CodegenT m Operand
    invokePrimOp op args' = do
      lookupPrimOp op >>= \case
        Left fn ->
          call fn =<< traverse snd args'
        Right compareInstr -> case args' of
          [(a, lhs), (b, rhs)] -> do
            valueA <- loadIfNeeded lhs a
            valueB <- loadIfNeeded rhs b
            compareInstr valueA valueB
          _ ->
            panic "Unexpected amount of arguments in 'invokePrimOp'!"

-- lowerM is a recursion-scheme that behaves like a zygomorphism, but it is
-- enhanced in the sense that both functions passed to the zygomorphism have
-- access to the original subtree.
--
-- NOTE: zygo effect is kind of abused here, since due to lazyness we can choose what
-- we need to compile to LLVM: instructions either return "()" or an "Operand".
-- para effect is needed since we need access to the original subtree in the
-- assignment case to check if we are assigning to a variable or not, allowing
-- us to easily transform an "expression-oriented" EIR to statement based LLVM IR.
lowerM :: (EIRF (EIR, CodegenT m Operand) -> CodegenT m Operand)
       -> (EIRF (Triple EIR (CodegenT m Operand) (CodegenT m ())) -> CodegenT m ())
       -> EIR
       -> CodegenT m ()
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

-- We need an Int somewhere later on during codegen.
-- So we don't convert to a 'Suffix' at this point yet.
type IntSuffix = Int
type CacheT = StateT (Map Metadata (IntSuffix, Table))

runCacheT :: Monad m => CacheT m a -> m (Map Metadata IntSuffix, a)
runCacheT m = do
  (a, s) <- runStateT m mempty
  pure (map fst s, a)

codegenRuntime :: Externals -> Metadata -> CacheT (ConfigT (ModuleBuilderT IO)) Table
codegenRuntime exts meta = gets (M.lookup meta) >>= \case
  Nothing -> do
    suffix <- gets length
    fns <- cgRuntime suffix
    modify $ M.insert meta (suffix, fns)
    pure fns
  Just (_, cachedFns) -> pure cachedFns
  where
    cgRuntime suffix = lift $ case meta of
      BTree meta' -> hoist (instantiate (show suffix) meta') $ BTree.codegen exts

codegenDebugInfos :: MonadModuleBuilder m => Map Metadata Int -> m ()
codegenDebugInfos metaMapping =
  traverse_ (uncurry codegenDebugInfo) $ M.toList metaMapping
  where
    codegenDebugInfo meta i =
      let hash = getHash meta
          name = Name $ ("specialize_debug_info." <>) $ unHash hash
       in global name i32 (Int 32 $ toInteger i)

codegenSymbolTable :: Externals -> ConfigT (ModuleBuilderT IO) (SymbolTable.SymbolTable, Symbol.Symbol)
codegenSymbolTable exts = do
  symbol <- lift $ hoist intoIO $ Symbol.codegen exts

  let tySymbol = Symbol.tySymbol symbol
      symbolDestructor iterPtr = do
        _ <- call (Symbol.symbolDestroy symbol) [iterPtr]
        pass

  -- Only this vector does the cleanup of all the symbols, to prevent double frees
  vec <- hoist (instantiate "symbol" tySymbol) $ Vector.codegen exts (Just symbolDestructor)
  hashMap <- HashMap.codegen symbol exts
  symbolTable <- lift $ hoist intoIO $ SymbolTable.codegen tySymbol vec hashMap
  pure (symbolTable, symbol)
  where
    intoIO = pure . runIdentity

getIndexFromMeta :: Metadata -> Index
getIndexFromMeta = \case
  BTree meta -> Index $ BTree.index meta
