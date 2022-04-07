module Eclair.Lowering.EIR
  ( compileToLLVM
  ) where

import Protolude hiding (Type, and, void)
import Control.Arrow ((&&&))
import Data.Functor.Foldable hiding (fold)
import Data.ByteString.Short hiding (index)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List ((!!))
import Data.Maybe (fromJust)
import LLVM.AST (Module)
import LLVM.AST.Operand hiding (Metadata)
import LLVM.AST.Name
import LLVM.AST.Type
import LLVM.AST.Constant hiding (index)
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Combinators
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.LLVM.BTree as BTree
import Eclair.LLVM.Metadata
import Eclair.LLVM.Codegen
import Eclair.RA.IndexSelection
import Eclair.Syntax


-- TODO: refactor this entire code, ...

type EIR = EIR.EIR
type EIRF = EIR.EIRF
type Relation = EIR.Relation

compileToLLVM :: EIR -> IO Module
compileToLLVM = \case
  EIR.Block (EIR.DeclareProgram metas : decls) -> buildModuleT "eclair_program" $ do
    exts <- createExternals
    fnss <- traverse (codegenRuntime exts . snd) metas
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
            getType ty = runReaderT (toLLVMType unusedRelation unusedIndex ty) beginState
        argTypes <- liftIO $ traverse getType tys
        returnType <- liftIO $ getType retTy
        let args = zipWith mkArg [0..] argTypes
        function (mkName $ T.unpack name) args returnType $ \args -> do
          runReaderT (fnBodyToLLVM args body) beginState
      _ ->
        panic "Unexpected top level EIR declaration when compiling to LLVM!"

-- NOTE: zygo is kind of abused here, since due to lazyness we can choose what we need
-- to  compile to LLVM: instructions either return "()" or an "Operand".
fnBodyToLLVM :: [Operand] -> EIR -> CodegenM ()
fnBodyToLLVM args = zygo instrToOperand instrToUnit
  where
    instrToOperand :: EIRF (CodegenM Operand) -> CodegenM Operand
    instrToOperand = \case
      EIR.FunctionArgF pos ->
        pure $ args !! pos
      EIR.FieldAccessF structOrVar pos -> do
        -- NOTE: structOrVar is always a pointer to a heap-/stack-allocated
        -- value so we need to first deref the pointer, and then index into the
        -- fields of the value ('addr' does this for us).
        addr (mkPath [int32 $ toInteger pos]) =<< structOrVar
      EIR.VarF v ->
        -- TODO: can we use `named` here? will it update everywhere?
        -- TODO: where do we put a value in the map? do we need "para" effect also?
        asks (fromJust . M.lookup v . varMap)
      EIR.NotF bool ->
        not' =<< bool
      EIR.AndF bool1 bool2 -> do
        b1 <- bool1
        b2 <- bool2
        and b1 b2
      EIR.EqualsF lhs rhs -> do
        a <- lhs
        b <- rhs
        icmp IP.EQ a b
      EIR.CallF r idx fn args ->
        doCall r idx fn args
      EIR.HeapAllocateProgramF -> do
        (malloc, programTy) <- asks (extMalloc . externals &&& programType)
        let programSize = ConstantOperand $ sizeof programTy
        pointer <- call malloc [(programSize, [])]
        pointer `bitcast` ptr programTy
      EIR.StackAllocateF r idx ty -> do
        theType <- toLLVMType r idx ty
        alloca theType (Just (int32 1)) 0
      EIR.LitF value ->
        pure $ int32 (fromIntegral value)
      _ ->
        panic "Unhandled pattern match case in 'instrToOperand' while lowering EIR to LLVM!"
    instrToUnit :: EIRF (CodegenM Operand, CodegenM ()) -> CodegenM ()
    instrToUnit = \case
      EIR.BlockF stmts ->
        traverse_ snd stmts
      EIR.ParF stmts ->
        -- NOTE: this is just sequential evaluation for now
        traverse_ snd stmts
      EIR.AssignF (fst -> operand) (fst -> val) -> do
        -- TODO use `named` combinator, store var in varMap
        -- TODO: what if we are assigning to field in struct? inspect var result?
        address <- operand
        value <- val
        store value 0 address
      EIR.FreeProgramF (fst -> programVar) -> do
        freeFn <- asks (extFree . externals)
        program <- programVar
        () <$ call freeFn [(program, [])]
      EIR.CallF r idx fn (map fst -> args) ->
        () <$ doCall r idx fn args
      EIR.LoopF stmts ->
        loop $ traverse_ snd stmts
      EIR.IfF (fst -> cond) (snd -> body) -> do
        condition <- cond
        if' condition body
      EIR.JumpF lbl ->
        br (labelToName lbl)
      EIR.LabelF lbl ->
        -- NOTE: the label should be globally unique thanks to the RA -> EIR lowering pass
        emitBlockStart $ labelToName lbl
      EIR.ReturnF (fst -> value) ->
        ret =<< value
      _ ->
        panic "Unhandled pattern match case in 'instrToUnit' while lowering EIR to LLVM!"
    doCall :: Relation -> Index -> EIR.Function -> [CodegenM Operand] -> CodegenM Operand
    doCall r idx fn args = do
      argOperands <- sequence args
      func <- lookupFunction r idx fn
      call func $ (, []) <$> argOperands

-- TODO: use caching, return cached compilation?
codegenRuntime :: Externals -> Metadata -> ModuleBuilderT IO Functions
codegenRuntime exts = \case
  BTree meta -> BTree.codegen exts meta

-- TODO: add hash?
mkType :: Name -> [Functions] -> ModuleBuilderT IO Type
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

