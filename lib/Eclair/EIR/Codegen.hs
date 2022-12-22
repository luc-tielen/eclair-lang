module Eclair.EIR.Codegen
  ( CodegenT
  , runCodegenM
  , LowerState(..)
  , Table(..)
  , Externals(..)
  , labelToName
  , lookupFunction
  , lookupPrimOp
  , toLLVMType
  , lookupVar
  , addVarBinding
  , newGlobalVarName
  , loadIfNeeded
  ) where

import Prelude hiding (void)
import Control.Monad.Morph
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Eclair.LLVM.Codegen
import Eclair.LLVM.Table
import Eclair.LLVM.Externals
import qualified Eclair.LLVM.Symbol as Symbol
import qualified Eclair.LLVM.SymbolTable as SymbolTable
import qualified Eclair.EIR.IR as EIR
import Eclair.RA.IndexSelection
import Eclair.Common.Id


type Relation = EIR.Relation
type EIR = EIR.EIR

type VarMap = Map Text Operand
type TableMap = Map (Relation, Index) Table

data LowerState
  = LowerState
  { programType :: Type
  , programSizeBytes :: Word64
  , symbolTableFns :: SymbolTable.SymbolTable
  , symbolFns :: Symbol.Symbol
  , fnsMap :: TableMap
  , varMap :: VarMap
  , globalVarCounter :: Int
  , externals :: Externals
  }

type CodegenT m = StateT LowerState (IRBuilderT (ModuleBuilderT m))


runCodegenM :: Monad m => CodegenT m a -> LowerState -> IRBuilderT (ModuleBuilderT m) a
runCodegenM = evalStateT

labelToName :: EIR.LabelId -> Name
labelToName (EIR.LabelId lbl) =
  Name lbl

-- This is a function mostly used by `lookupPrimOp`, but also for calling functions during fact IO
lookupFunction :: Monad m => Relation -> Index -> EIR.Function -> CodegenT m Operand
lookupFunction r idx fn = do
  tableMap <- gets fnsMap
  let table = unsafeLookup r idx tableMap
  extractFn tableMap table
  where
    extractFn tableMap table = case fn of
      EIR.InitializeEmpty -> pure $ fnInitEmpty table
      EIR.Destroy -> pure $ fnDestroy table
      EIR.Purge -> pure $ fnPurge table
      EIR.Swap -> pure $ fnSwap table
      EIR.InsertRange r2 idx2 -> do
        let templatedFn = fnInsertRangeTemplate table
            suffix = unId r <> "_" <> unId r2
            table2 = unsafeLookup r2 idx2 tableMap
            iterParams = IteratorParams
              { ipIterCurrent = fnIterCurrent table2
              , ipIterNext = fnIterNext table2
              , ipIterIsEqual = fnIterIsEqual table2
              , ipTypeIter = typeIter table2
              }
        -- I think we don't need to cache instantiations here (to avoid duplicate functions)
        lift . lift $ hoist generalize $ instantiate suffix iterParams templatedFn
      EIR.IsEmpty -> pure $ fnIsEmpty table
      EIR.Size -> pure $ fnSize table
      EIR.Contains -> pure $ fnContains table
      EIR.Insert -> pure $ fnInsert table
      EIR.IterCurrent -> pure $ fnIterCurrent table
      EIR.IterNext -> pure $ fnIterNext table
      EIR.IterIsEqual -> pure $ fnIterIsEqual table
      EIR.IterLowerBound -> pure $ fnLowerBound table
      EIR.IterUpperBound -> pure $ fnUpperBound table
      EIR.IterBegin -> pure $ fnBegin table
      EIR.IterEnd -> pure $ fnEnd table

    unsafeLookup r' idx' = fromJust . M.lookup (r', idx')

type PrimOp m = Either Operand (Operand -> Operand -> CodegenT m Operand)
lookupPrimOp :: Monad m => EIR.Op -> CodegenT m (PrimOp m)
lookupPrimOp = \case
  EIR.SymbolTableInit ->
    toSymbolTableOp SymbolTable.symbolTableInit
  EIR.SymbolTableDestroy ->
    toSymbolTableOp SymbolTable.symbolTableDestroy
  EIR.SymbolTableInsert ->
    toSymbolTableOp SymbolTable.symbolTableFindOrInsert
  EIR.RelationOp r idx fn ->
    Left <$> lookupFunction r idx fn
  EIR.ComparisonOp op ->
    pure $ Right $ case op of
      EIR.Equals -> eq
      EIR.NotEquals -> ne
      -- NOTE: this will result in issues for signed integers in the future, but ignoring that for now..
      -- We can pass along the args then?
      EIR.LessThan -> ult
      EIR.LessOrEqual -> ule
      EIR.GreaterThan -> ugt
      EIR.GreaterOrEqual -> uge
  where
    toSymbolTableOp llvmOp = Left <$> do
      symbolTable <- gets symbolTableFns
      pure $ llvmOp symbolTable

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
lookupVar :: Monad m => Text -> CodegenT m Operand
lookupVar v = gets (fromJust . M.lookup v . varMap)

addVarBinding :: Monad m => Text -> Operand -> CodegenT m ()
addVarBinding var value =
  modify $ \s -> s { varMap = M.insert var value (varMap s) }

newGlobalVarName :: Monad m => Text -> CodegenT m Name
newGlobalVarName name = do
  count <- gets globalVarCounter
  modify $ \s -> s { globalVarCounter = count + 1 }
  pure $ Name $ name <> "_" <> show count

-- NOTE: this is for the case when we are assigning 1 field of a struct/array
-- to another of the same kind, where the right side needs to be loaded before
-- storing it to the left side of the equation.
loadIfNeeded :: Monad m => CodegenT m Operand -> EIR -> CodegenT m Operand
loadIfNeeded operand = \case
  EIR.FieldAccess _ _ -> flip load 0 =<< operand
  _ -> operand
