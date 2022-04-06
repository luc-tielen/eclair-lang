module Eclair.LLVM.Codegen
  ( Functions(..)
  , mkObject
  , mkIter
  , mkValue
  ) where

import Protolude hiding (Type)
import LLVM.AST.Operand (Operand)
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction hiding (store)
import LLVM.IRBuilder.Constant
import LLVM.AST.Type

-- Like a vtable in C++, except here everything is guaranteed to be inlined
-- because of specialization.
data Functions
  = Functions
  { fnInit :: Operand
  , fnInitEmpty :: Operand
  , fnDestroy :: Operand
  , fnPurge :: Operand
  , fnSwap :: Operand
  , fnBegin :: Operand
  , fnEnd :: Operand
  , fnIsEmpty :: Operand
  , fnLowerBound :: Operand
  , fnUpperBound :: Operand
  , fnContains :: Operand
  , fnInsert :: Operand
  , fnInsertRange :: Operand
  , fnIterIsEqual :: Operand
  , fnIterCurrent :: Operand
  , fnIterNext :: Operand
  , typeObj :: Type
  , typeIter :: Type
  , typeValue :: Type
  }

mkObject :: MonadIRBuilder m => Functions -> m Operand
mkObject fns = allocaTy (typeObj fns)

mkIter :: MonadIRBuilder m => Functions -> m Operand
mkIter fns = allocaTy (typeIter fns)

mkValue :: MonadIRBuilder m => Functions -> m Operand
mkValue fns = allocaTy (typeValue fns)

allocaTy :: MonadIRBuilder m => Type -> m Operand
allocaTy ty = alloca ty (Just (int32 1)) 0
