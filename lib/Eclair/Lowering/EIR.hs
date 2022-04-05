module Eclair.Lowering.EIR
  () where

import Protolude hiding (and, void)
import Data.Functor.Foldable hiding (fold)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List ((!!))
import Data.Maybe (fromJust)
import qualified Eclair.EIR.IR as EIR
import LLVM.AST (Module)
import LLVM.AST.Operand
import LLVM.AST.Name
import LLVM.AST.Type
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Combinators

type EIR = EIR.EIR
type EIRF = EIR.EIRF

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
  EIR.Block stmts -> buildModuleT "eclair_program" $ do
    mallocFn <- extern "malloc" [i32] (ptr i8)
    freeFn <- extern "free" [ptr i8] void
    let externalMap = Externals mallocFn freeFn
    traverse_ (processDecl externalMap) stmts
  _ ->
    panic "Unexpected module level EIR declaration when compiling to LLVM!"
  where
    processDecl externalMap = \case
      EIR.DeclareProgram metas ->
        _
      EIR.Function name tys body ->
        function (mkName $ T.unpack name) _ _ $ \args -> do
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
      EIR.CallF fn args ->
        doCall fn args
      EIR.HeapAllocateProgramF ->
        -- TODO: call "malloc", return ptr
        -- TODO: lookup size of program type, or do 2x pointer size x number of relation types...
        _
      EIR.StackAllocateF ty r ->
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
      EIR.CallF fn args ->
        () <$ doCall fn args
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
    doCall fn args =
      -- TODO: look up corresponding fn -> how to know which to select?
      _

labelToName :: EIR.LabelId -> Name
labelToName (EIR.LabelId lbl) =
  mkName $ T.unpack lbl
