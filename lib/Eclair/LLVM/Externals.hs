module Eclair.LLVM.Externals
  ( Externals(..)
  ) where

import Eclair.LLVM.Codegen (Operand)

-- Functions that are defined outside of LLVM.
data Externals
  = Externals
  { extMalloc :: Operand
  , extFree   :: Operand
  , extMemset :: Operand
  , extMemcpy :: Operand
  , extMemcmp :: Operand
  , extMmap   :: Operand
  , extMunmap :: Operand
  }
