module Eclair.LLVM.Allocator.Page
  ( Page
  , allocator
  ) where

import Eclair.LLVM.Allocator.Common
import Eclair.LLVM.Codegen hiding (allocate)

data Page

-- TODO: parametrize on page size (add argument, pass to helper functions)
allocator :: Allocator 'Base Page
allocator
  = Stateless
  { slAlloc = allocatePages
  , slFree = freePages
  }

pageSize :: Operand
pageSize = int32 4096

allocatePages :: Operand -> AllocIRCodegenM Operand
allocatePages numBytes = do
  mmap <- gets extMmap
  numBytes' <- flip zext i64 =<< roundToNearestPageSize numBytes
  let hint = nullPtr VoidType
      protRead = int32 1
      protWrite = int32 2
      mapPrivate = int32 2
      mapAnonymous = int32 32
      noFd = int32 (-1)
      offset = int32 0
  prot <- protRead `or` protWrite  -- allow both reads and writes
  flags <- mapPrivate `or` mapAnonymous  -- anonymous private mapping, can be used as RAM
  call mmap [hint, numBytes', prot, flags, noFd, offset]

roundToNearestPageSize :: Operand -> AllocIRCodegenM Operand
roundToNearestPageSize numBytes = do
  x1 <- numBytes `add` pageSize
  x2 <- x1 `sub` int32 1
  y <- int32 0 `sub` pageSize
  x2 `and` y

freePages :: Operand -> Operand -> AllocIRCodegenM ()
freePages memory len = do
  munmap <- gets extMunmap
  len' <- zext len i64
  _ <- call munmap [memory, len']
  pass
