module Eclair.LLVM.Runtime
  ( Table(..)
  , Externals(..)
  ) where

import LLVM.Codegen

-- A data type representing all functionality of a Datalog table / container.
-- This is similar to a vtable in C++, except here everything is guaranteed to be inlined
-- because of specialization. Each of the operands refers to a different LLVM function.
-- The types are also "exported" because they are used in other parts of the code.
data Table
  = Table
  { fnInit :: Operand
  , fnInitEmpty :: Operand
  , fnDestroy :: Operand
  , fnPurge :: Operand
  , fnSwap :: Operand
  , fnBegin :: Operand
  , fnEnd :: Operand
  , fnIsEmpty :: Operand
  , fnSize :: Operand
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

-- Functions that are defined outside of LLVM.
data Externals
  = Externals
  { extMalloc :: Operand
  , extFree :: Operand
  , extMemset :: Operand
  , extMemcpy :: Operand
  , extMemcmp :: Operand
  }
