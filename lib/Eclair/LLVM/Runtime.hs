module Eclair.LLVM.Runtime
  ( Functions(..)
  , Externals(..)
  , Suffix
  , HasSuffix(..)
  ) where

import LLVM.Codegen

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

-- Functions that are defined outside of LLVM
data Externals
  = Externals
  { extMalloc :: Operand
  , extFree :: Operand
  , extMemset :: Operand
  }

-- Appended to every LLVM type and function to make sure no collisions occur during codegen.
type Suffix = Int

class HasSuffix a where
  getSuffix :: a -> Suffix
