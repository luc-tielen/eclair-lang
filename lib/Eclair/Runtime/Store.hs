module Eclair.Runtime.Store
  ( Store(..)
  , Object
  , Functions(..)
  ) where

import Protolude
import LLVM.AST.Operand (Operand)
import Eclair.RA.IndexSelection
import LLVM.IRBuilder.Monad
import Control.Monad.Morph

-- A stack allocated object.
type Object = Operand

-- Helper data type that manages multiple underlying datastructures.
-- This is useful for example when you need to update multiple indices,
-- but sometimes only want to query a specific one.
newtype Store
  = Store
  { objects :: Map Index (Object, Functions)  -- TODO: nonemptymap
  }

-- Like a vtable in C++, except here everything is guaranteed to be inlined
-- because of specialization.
data Functions
  = Functions
  { fnPurge :: Operand
  , fnSwap :: Operand
  , fnBegin :: Operand
  , fnEnd :: Operand
  , fnIsEmpty :: Operand
  , fnInsertRange :: Operand
  , fnAllocateObj :: Allocator
  , fnAllocateIter :: Allocator
  }
  -- TODO: store all functions as operands

type Allocator = forall t m. (MonadIRBuilder (t m), MonadIO m)
              => t m Operand

