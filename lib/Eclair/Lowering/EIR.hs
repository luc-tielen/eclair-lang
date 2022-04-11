module Eclair.Lowering.EIR
  ( compileToLLVM
  ) where

import Protolude hiding (Type, and, void)
import Control.Arrow ((&&&))
import Control.Comonad
import Data.Functor.Foldable hiding (fold)
import Data.ByteString.Short hiding (index, length)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.List as L
import Data.List ((!!))
import Data.Maybe (fromJust)
import LLVM.AST (Module)
import LLVM.AST.Operand hiding (Metadata)
import LLVM.AST.Name
import qualified LLVM.AST.Type as LLVM (Type)
import LLVM.AST.Type hiding (Type)
import LLVM.AST.Constant hiding (index)
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.IRBuilder.Instruction hiding (sizeof)
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Combinators
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.LLVM.BTree as BTree
import Eclair.LLVM.Metadata
import Eclair.LLVM.Codegen
import Eclair.LLVM.Hash
import Eclair.LLVM.Runtime
import Eclair.RA.IndexSelection
import Eclair.Syntax


type EIR = EIR.EIR
type EIRF = EIR.EIRF
type Relation = EIR.Relation

compileToLLVM :: EIR -> IO Module
compileToLLVM = \case
  EIR.Block (EIR.DeclareProgram metas : decls) -> buildModuleT "eclair_program" $ do
    exts <- createExternals
    (metaMapping, fnss) <- runCacheT $ traverse (codegenRuntime exts . snd) metas
    codegenDebugInfos metaMapping
    let fnsInfo = zip (map (map getIndexFromMeta) metas) fnss
        fnsMap = M.fromList fnsInfo
    programTy <- mkType "program" fnss
    traverse_ (processDecl programTy fnsMap exts) decls
  _ ->
    panic "Unexpected top level EIR declarations when compiling to LLVM!"
  where
    processDecl programTy fnsMap externalMap = \case
      EIR.Function name tys retTy body -> do
        let beginState = LowerState programTy fnsMap mempty externalMap
            unusedRelation = panic "Unexpected use of relation for function type when lowering EIR to LLVM."
            unusedIndex = panic "Unexpected use of index for function type when lowering EIR to LLVM."
            getType ty = evalStateT (toLLVMType unusedRelation unusedIndex ty) beginState
        argTypes <- liftIO $ traverse getType tys
        returnType <- liftIO $ getType retTy
        let args = map (, ParameterName "arg") argTypes
        function (mkName $ T.unpack name) args returnType $ \args -> do
          runCodegenM (fnBodyToLLVM args body) beginState
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
        icmp IP.EQ valueA valueB
      EIR.CallF r idx fn (map snd -> args) ->
        doCall r idx fn args
      EIR.HeapAllocateProgramF -> do
        (malloc, programTy) <- gets (extMalloc . externals &&& programType)
        memorySize <- sizeOfProgram programTy `named` "byte_count"
        pointer <- call malloc [(memorySize, [])] `named` "memory"
        pointer `bitcast` ptr programTy
      EIR.StackAllocateF r idx ty -> do
        theType <- toLLVMType r idx ty
        alloca theType (Just (int32 1)) 0
      EIR.LitF value ->
        pure $ int32 (fromIntegral value)
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
            value <- val `named` toShort (encodeUtf8 varName)
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
        () <$ call freeFn [(memory, [])]
      EIR.CallF r idx fn (map toOperand -> args) ->
        () <$ doCall r idx fn args
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
    doCall :: Relation -> Index -> EIR.Function -> [CodegenM Operand] -> CodegenM Operand
    doCall r idx fn args = do
      argOperands <- sequence args
      func <- lookupFunction r idx fn
      call func $ (, []) <$> argOperands
    toOperand (Triple _ operand _) = operand
    toOperandWithContext (Triple eir operand _) =
      (operand, eir)
    toInstrs (Triple _ _ instrs) = instrs

-- A tuple of 3 elements, defined as a newtype so a Comonad instance can be added.
data Triple a b c
  = Triple
  { tFst :: a
  , tSnd :: b
  , tThd :: c
  } deriving Functor

instance Comonad (Triple a b) where
  extract (Triple _ _ c) = c

  duplicate (Triple a b c) =
    Triple a b (Triple a b c)

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
lowerM f = gcata (distParaZygo f)
  where
    distParaZygo
      :: Corecursive t
      => (Base t (t, b) -> b)
      -> (Base t (Triple t b a) -> Triple t b (Base t a))
    distParaZygo g m =
      let base_t_t = map tFst m
          base_t_tb = map (tFst &&& tSnd) m
          base_t_a = map tThd m
       in Triple (embed base_t_t) (g base_t_tb) base_t_a

-- TODO: get cpu arch to make this check dynamically
sizeOfProgram :: LLVM.Type -> CodegenM Operand
sizeOfProgram programTy = do
  let ptrSizeBits = 64
      programSize = ConstantOperand $ sizeof ptrSizeBits programTy
  -- TODO: use ADT to make pattern match exhaustive
  -- case ptrSizeBits of
  --   32 -> pure programSize
  --   64 -> trunc programSize i32
  trunc programSize i32

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
          name = mkName $ T.unpack $ ("specialize_debug_info." <>) $ unHash hash
       in global name i32 (Int 32 $ toInteger i)

-- TODO: add hash based on filepath of the file we're compiling?
mkType :: Name -> [Functions] -> ModuleBuilderT IO LLVM.Type
mkType name fnss =
  typedef name (struct tys)
  where
    struct = Just . StructureType False
    tys = map typeObj fnss

getIndexFromMeta :: Metadata -> Index
getIndexFromMeta = \case
  BTree meta -> Index $ BTree.index meta

createExternals :: ModuleBuilderT IO Externals
createExternals = do
  mallocFn <- extern "malloc" [i32] (ptr i8)
  freeFn <- extern "free" [ptr i8] void
  memsetFn <- extern "llvm.memset.p0i8.i64" [ptr i8, i8, i64, i1] void
  pure $ Externals mallocFn freeFn memsetFn
