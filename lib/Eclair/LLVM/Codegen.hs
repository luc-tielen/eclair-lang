module Eclair.LLVM.Codegen
  ( CodegenM(..)
  , runCodegenM
  , LowerState(..)
  , Externals(..)
  , Functions(..)
  , mkArg
  , labelToName
  , lookupFunction
  , toLLVMType
  ) where

import Protolude hiding (Type, void)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Map as M
import LLVM.AST.Operand (Operand)
import LLVM.AST.Type (Type, ptr, void)
import LLVM.AST.Name
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import Data.ByteString.Short hiding (index)
import Eclair.LLVM.Runtime
import qualified Eclair.EIR.IR as EIR
import Eclair.RA.IndexSelection


type Relation = EIR.Relation

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

mkArg :: Word8 -> Type -> (Type, ParameterName)
mkArg x ty =
  (ty, ParameterName $ "arg" <> pack [x])

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
