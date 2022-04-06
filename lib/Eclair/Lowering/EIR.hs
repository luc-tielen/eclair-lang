module Eclair.Lowering.EIR
  ( compileEIR
  ) where

import Protolude hiding (Type, and, void)
import Data.Functor.Foldable hiding (fold)
import Data.ByteString.Short
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List ((!!))
import Data.Maybe (fromJust)
import LLVM.AST (Module)
import LLVM.AST.Operand hiding (Metadata)
import LLVM.AST.Name
import LLVM.AST.Type
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Combinators
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.LLVM.BTree as BTree
import Eclair.LLVM.Metadata
import Eclair.RA.IndexSelection


type EIR = EIR.EIR
type EIRF = EIR.EIRF
type Relation = EIR.Relation

type VarMap = Map Text Operand

data Externals
  = Externals
  { extMalloc :: Operand
  , extFree :: Operand
  }

data LowerState
  = LowerState
  { varMap :: VarMap
  , externals :: Externals
  }

type CodegenM = ReaderT LowerState (IRBuilderT (ModuleBuilderT IO))

-- TODO generate LLVM code for all the types, carry around that info in CodegenM

compileEIR :: EIR -> IO Module
compileEIR = \case
  EIR.Block (EIR.DeclareProgram metas : decls) -> buildModuleT "eclair_program" $ do
    mallocFn <- extern "malloc" [i32] (ptr i8)
    freeFn <- extern "free" [ptr i8] void
    let externalMap = Externals mallocFn freeFn
    -- TODO: codegen btree etc from metadatas
    -- TODO: group Functions + Sizes together with other essential information (Relation + Index?)
    let mkType :: Text -> [Metadata] -> ModuleBuilderT IO Type
        mkType name metas = _
    programType <- mkType "program" metas

    let programType :: Type
        programType = _
    traverse_ (processDecl programType externalMap) decls
  _ ->
    panic "Unexpected top level EIR declarations when compiling to LLVM!"
  where
    processDecl programType externalMap = \case
      EIR.Function name tys retTy body -> do
        argTypes <- liftIO $ traverse (toLLVMType programType) tys
        returnType <- liftIO $ toLLVMType programType retTy
        let args = zipWith mkArg [0..] argTypes
        function (mkName $ T.unpack name) args returnType $ \args -> do
          runReaderT (fnBodyToLLVM args body) (LowerState mempty externalMap)
      _ ->
        panic "Unexpected top level EIR declaration when compiling to LLVM!"


-- TODO: no IO
-- TODO: make type more general, remove modulebuilder part

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
      EIR.HeapAllocateProgramF ->
        -- TODO: call "malloc", return ptr
        -- TODO: lookup size of program type, or do 2x pointer size x number of relation types...
        _
      EIR.StackAllocateF r idx ty ->
        -- TODO: use alloca, lookup size info based on relation
        _
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

lookupFunction :: Relation -> Index -> EIR.Function -> CodegenM Operand
lookupFunction r idx fn = do
  -- TODO: lookup function from functions object
  let extractFn = case fn of
        EIR.InitializeEmpty -> _
        EIR.Destroy -> _
        EIR.Purge -> _
        EIR.Swap -> _
        EIR.InsertRange -> _
        EIR.IsEmpty -> _
        EIR.Contains -> _
        EIR.Insert -> _
        EIR.IterCurrent -> _
        EIR.IterNext -> _
        EIR.IterIsEqual -> _
        EIR.IterLowerBound -> _
        EIR.IterUpperBound -> _
        EIR.IterBegin -> _
        EIR.IterEnd -> _
  extractFn <$> _ -- TODO use r+idx to lookup matching functions in LowerState

labelToName :: EIR.LabelId -> Name
labelToName (EIR.LabelId lbl) =
  mkName $ T.unpack lbl

toLLVMType :: Type -> EIR.Type -> IO Type
toLLVMType programType = \case
  -- TODO: look up types in the codegen monad..
  EIR.Program -> pure programType
  EIR.Iter -> _  -- TODO: depends on r + idx
  EIR.Value -> _  -- TODO: depends on r + idx
  EIR.Void -> pure void
  EIR.Pointer ty -> ptr <$> toLLVMType programType ty

mkArg :: Word8 -> Type -> (Type, ParameterName)
mkArg x ty =
  (ty, ParameterName $ "arg" <> pack [x])

