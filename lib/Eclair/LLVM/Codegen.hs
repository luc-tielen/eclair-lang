module Eclair.LLVM.Codegen
  ( CodegenM(..)
  , runCodegenM
  , LowerState(..)
  , Externals(..)
  , Functions(..)
  , labelToName
  , lookupFunction
  , toLLVMType
  , lookupVar
  , addVarBinding
  , loadIfNeeded
  ) where

import Prelude hiding (Type, void)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Map as M
import LLVM.AST.Operand (Operand)
import LLVM.AST.Type (Type, ptr, void)
import LLVM.AST.Name
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import Data.ByteString.Short hiding (index)
import Eclair.LLVM.Runtime
import qualified Eclair.EIR.IR as EIR
import Eclair.RA.IndexSelection


type Relation = EIR.Relation
type EIR = EIR.EIR

type VarMap = Map Text Operand
type FunctionsMap = Map (Relation, Index) Functions

data LowerState
  = LowerState
  { programType :: Type
  , fnsMap :: FunctionsMap
  , varMap :: VarMap
  , externals :: Externals
  }

type CodegenM = StateT LowerState (IRBuilderT (ModuleBuilderT IO))


runCodegenM :: CodegenM a -> LowerState -> IRBuilderT (ModuleBuilderT IO) a
runCodegenM = evalStateT

labelToName :: EIR.LabelId -> Name
labelToName (EIR.LabelId lbl) =
  mkName $ T.unpack lbl

lookupFunction :: Relation -> Index -> EIR.Function -> CodegenM Operand
lookupFunction r idx fn =
  extractFn . fromJust . M.lookup (r, idx) <$> gets fnsMap
  where
    extractFn = case fn of
      EIR.InitializeEmpty -> fnInitEmpty
      EIR.Destroy -> fnDestroy
      EIR.Purge -> fnPurge
      EIR.Swap -> fnSwap
      EIR.InsertRange -> fnInsertRange
      EIR.IsEmpty -> fnIsEmpty
      EIR.Contains -> fnContains
      EIR.Insert -> fnInsert
      EIR.IterCurrent -> fnIterCurrent
      EIR.IterNext -> fnIterNext
      EIR.IterIsEqual -> fnIterIsEqual
      EIR.IterLowerBound -> fnLowerBound
      EIR.IterUpperBound -> fnUpperBound
      EIR.IterBegin -> fnBegin
      EIR.IterEnd -> fnEnd

toLLVMType :: (MonadState LowerState m, MonadIO m)
           => Relation -> Index -> EIR.Type -> m Type
toLLVMType r idx = go
  where
    go = \case
      EIR.Program ->
        programType <$> get
      EIR.Iter ->
        typeIter . fromJust . M.lookup (r, idx) <$> gets fnsMap
      EIR.Value ->
        typeValue . fromJust . M.lookup (r, idx) <$> gets fnsMap
      EIR.Void ->
        pure void
      EIR.Pointer ty ->
        ptr <$> go ty

-- Only called internally, should always be called on a var that exists.
lookupVar :: Text -> CodegenM Operand
lookupVar v = gets (fromJust . M.lookup v . varMap)

addVarBinding :: Text -> Operand -> CodegenM ()
addVarBinding var value =
  modify $ \s -> s { varMap = M.insert var value (varMap s) }

-- NOTE: this is for the case when we are assigning 1 field of a struct/array
-- to another of the same kind, where the right side needs to be loaded before
-- storing it to the left side of the equation.
loadIfNeeded :: CodegenM Operand -> EIR -> CodegenM Operand
loadIfNeeded operand = \case
  EIR.FieldAccess _ _ -> flip load 0 =<< operand
  _ -> operand
