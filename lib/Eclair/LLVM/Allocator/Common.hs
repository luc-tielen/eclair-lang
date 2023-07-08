module Eclair.LLVM.Allocator.Common
  ( Allocator(..)
  , AllocCodegenM
  , AllocIRCodegenM
  , module Eclair.LLVM.Externals
  ) where

import Eclair.LLVM.Codegen
import Eclair.LLVM.Externals

type AllocCodegenM = StateT Externals ModuleBuilder
type AllocIRCodegenM = IRBuilderT AllocCodegenM

data Allocator inner
  = Allocator
  { cgType :: Maybe (inner -> AllocCodegenM Type)
  , cgAlloc :: Operand -> AllocIRCodegenM Operand
  , cgFree :: Operand  -- ptr
           -> Operand  -- length
           -> AllocIRCodegenM ()
  -- TODO: initialize, destroy (stateful allocators)
  -- TODO: , cgResize :: Operand -> Operand -> AllocIRCodegenM ()
  , backingAllocator :: Maybe inner
  }
