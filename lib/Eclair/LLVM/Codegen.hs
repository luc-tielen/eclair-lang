module Eclair.LLVM.Codegen
  ( Functions(..)
  ) where

import Protolude hiding (Type)
import LLVM.AST.Operand (Operand)
import LLVM.AST.Type (Type)

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

