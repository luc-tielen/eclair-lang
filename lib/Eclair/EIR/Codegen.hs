module Eclair.EIR.Codegen
  ( CodegenM(..)
  , runCodegenM
  , LowerState(..)
  , Externals(..)
  , Functions(..)
  , labelToName
  , lsLookupFunction
  , lookupFunction
  , lookupPrimOp
  , toLLVMType
  , lookupVar
  , addVarBinding
  , newGlobalVarName
  , loadIfNeeded
  ) where

import Prelude hiding (void)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.ByteString.Short hiding (index)
import LLVM.Codegen
import Eclair.LLVM.Runtime
import qualified Eclair.LLVM.Symbol as Symbol
import qualified Eclair.LLVM.SymbolTable as SymbolTable
import qualified Eclair.EIR.IR as EIR
import Eclair.RA.IndexSelection
import Eclair.Id


type Relation = EIR.Relation
type EIR = EIR.EIR

type VarMap = Map Text Operand
type FunctionsMap = Map (Relation, Index) Functions

data LowerState
  = LowerState
  { programType :: Type
  , programSizeBytes :: Word64
  , symbolTableFns :: SymbolTable.SymbolTable
  , symbolFns :: Symbol.Symbol
  , fnsMap :: FunctionsMap
  , varMap :: VarMap
  , globalVarCounter :: Int
  , externals :: Externals
  }

type CodegenM = StateT LowerState (IRBuilderT (ModuleBuilderT IO))


runCodegenM :: CodegenM a -> LowerState -> IRBuilderT (ModuleBuilderT IO) a
runCodegenM = evalStateT

labelToName :: EIR.LabelId -> Name
labelToName (EIR.LabelId lbl) =
  Name lbl

-- This is a function mostly used by `lookupFunction`, but also for calling functions during fact IO
lsLookupFunction :: Relation -> Index -> EIR.Function -> LowerState -> Operand
lsLookupFunction r idx fn lowerState =
  extractFn . fromJust . M.lookup (r, idx) $ fnsMap lowerState
  where
    extractFn = case fn of
      EIR.InitializeEmpty -> fnInitEmpty
      EIR.Destroy -> fnDestroy
      EIR.Purge -> fnPurge
      EIR.Swap -> fnSwap
      EIR.InsertRange -> fnInsertRange
      EIR.IsEmpty -> fnIsEmpty
      EIR.Size -> fnSize
      EIR.Contains -> fnContains
      EIR.Insert -> fnInsert
      EIR.IterCurrent -> fnIterCurrent
      EIR.IterNext -> fnIterNext
      EIR.IterIsEqual -> fnIterIsEqual
      EIR.IterLowerBound -> fnLowerBound
      EIR.IterUpperBound -> fnUpperBound
      EIR.IterBegin -> fnBegin
      EIR.IterEnd -> fnEnd

lookupFunction :: Relation -> Index -> EIR.Function -> CodegenM Operand
lookupFunction r idx fn =
  gets (lsLookupFunction r idx fn)

lookupPrimOp :: EIR.Op -> CodegenM Operand
lookupPrimOp = \case
  EIR.SymbolTableInit -> do
    symbolTable <- gets symbolTableFns
    pure $ SymbolTable.symbolTableInit symbolTable
  EIR.SymbolTableDestroy -> do
    symbolTable <- gets symbolTableFns
    pure $ SymbolTable.symbolTableDestroy symbolTable
  EIR.SymbolTableInsert -> do
    symbolTable <- gets symbolTableFns
    pure $ SymbolTable.symbolTableFindOrInsert symbolTable

toLLVMType :: (MonadState LowerState m) => Relation -> Index -> EIR.Type -> m Type
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

newGlobalVarName :: Text -> CodegenM Name
newGlobalVarName name = do
  count <- gets globalVarCounter
  modify $ \s -> s { globalVarCounter = count + 1 }
  pure $ Name $ name <> "_" <> show count

-- NOTE: this is for the case when we are assigning 1 field of a struct/array
-- to another of the same kind, where the right side needs to be loaded before
-- storing it to the left side of the equation.
loadIfNeeded :: CodegenM Operand -> EIR -> CodegenM Operand
loadIfNeeded operand = \case
  EIR.FieldAccess _ _ -> flip load 0 =<< operand
  _ -> operand
