module Eclair.LLVM.Allocator.Malloc
  ( Malloc
  , allocator
  ) where

import Eclair.LLVM.Allocator.Common
import Eclair.LLVM.Codegen

data Malloc

allocator :: Allocator Malloc
allocator
  = Allocator
  { aType = mkType
  , aInit = const pass
  , aDestroy = const pass
  , aAlloc = const allocFn
  , aFree = const freeFn
  , aKind = Root
  , aInner = None
  }

mkType :: Text -> AllocCodegenM Type
mkType prefix =
  typedef (Name $ prefix <> "mallocator") Off []

allocFn :: Operand -> AllocIRCodegenM Operand
allocFn numBytes = do
  malloc <- gets extMalloc
  call malloc [numBytes]

freeFn :: Operand -> Operand -> AllocIRCodegenM ()
freeFn memory _ = do
  free <- gets extFree
  _ <- call free [memory]
  pass
