{-# LANGUAGE GADTs #-}

module Eclair.LLVM.Allocator.Arena
  ( Arena
  , allocator
  ) where

import Prelude hiding (void)
import Eclair.LLVM.Allocator.Common
import Eclair.LLVM.Codegen

data Arena a

-- TODO support multiple implementations, switch based on an enum:
-- - variant that only allocates, never frees until destroyed
-- - variant that can free sometimes, when allocation counter hits 0
-- - bump up vs bump down (each has it's own benefits): https://fitzgeraldnick.com/2019/11/01/always-bump-downwards.html

allocator :: Int -> Allocator a -> Allocator (Arena a)
allocator arenaSize inner
  = Allocator
  { aType = mkType
  , aInit = arenaInit arenaSize
  , aDestroy = arenaDestroy arenaSize
  , aAlloc = arenaAlloc
  , aFree = arenaFree
  , aKind = Node
  , aInner = inner
  }

mkType :: Text -> Type -> AllocCodegenM Type
mkType prefix baseTy =
  typedef (Name $ prefix <> "arena_allocator") Off
    [ baseTy    -- inner allocator
    , ptr void  -- start
    , ptr void  -- current
    ]

innerPtr :: Operand -> AllocIRCodegenM Operand
innerPtr = addr innerAllocOf

arenaInit :: Int -> VTable -> InitFn
arenaInit arenaSize fns alloc = do
  inner <- innerPtr alloc
  vtInit fns inner

  let size = int32 $ fromIntegral arenaSize
  memory <- vtAllocate fns inner size
  didAllocationFail <- memory `eq` nullPtr void
  if' didAllocationFail $ do
    assign startPtrOf alloc (nullPtr void)
    assign currentPtrOf alloc (nullPtr void)
    retVoid

  memoryEnd <- gep memory [size]
  assign startPtrOf alloc memory
  -- NOTE: we start at end and bump down, this can be implemented faster
  -- https://fitzgeraldnick.com/2019/11/01/always-bump-downwards.html
  assign currentPtrOf alloc memoryEnd

arenaDestroy :: Int -> VTable -> DestroyFn
arenaDestroy arenaSize fns alloc = do
  startPtr <- deref startPtrOf alloc
  isNull <- startPtr `eq` nullPtr void
  if' isNull $ do
    vtDestroy fns =<< innerPtr alloc
    retVoid

  inner <- innerPtr alloc
  let numBytes = int32 $ fromIntegral arenaSize
  vtDeallocate fns inner startPtr numBytes

  vtDestroy fns =<< innerPtr alloc
  assign startPtrOf alloc (nullPtr void)
  assign currentPtrOf alloc (nullPtr void)

arenaAlloc :: VTable -> AllocateFn
arenaAlloc _ alloc numBytes = do
  startPtr <- deref startPtrOf alloc
  currentPtr <- deref currentPtrOf alloc

  numBytesNegated <- sub (int32 0) numBytes
  newPtr <- gep currentPtr [numBytesNegated]
  newAddr <- ptrtoint newPtr i64
  startAddr <- ptrtoint startPtr i64
  noSpaceLeft <- newAddr `ult` startAddr

  if' noSpaceLeft $ do
    pure $ nullPtr void

  assign currentPtrOf alloc newPtr
  pure newPtr

-- Arena can't free individual pieces of memory, only everything at once
arenaFree :: VTable -> DeallocateFn
arenaFree _ _ _ _ = pass

data Paths
  = Alloc
  | InnerAlloc
  | StartPtr
  | CurrentPtr

innerAllocOf :: Path Alloc InnerAlloc
innerAllocOf = mkPath [int32 0]

startPtrOf :: Path Alloc StartPtr
startPtrOf = mkPath [int32 1]

currentPtrOf :: Path Alloc CurrentPtr
currentPtrOf = mkPath [int32 2]
