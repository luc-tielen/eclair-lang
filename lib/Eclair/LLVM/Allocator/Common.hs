{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs, TypeFamilyDependencies, PolyKinds #-}

module Eclair.LLVM.Allocator.Common
  ( Allocator(..)
  , Alloc(..)
  , AllocatorKind(..)
  , AllocatorKindTag(..)
  , VTable(..)
  , None(..)
  , cgAlloc
  , AllocCodegenM
  , AllocIRCodegenM
  , InitFn
  , DestroyFn
  , AllocateFn
  , DeallocateFn
  , module Eclair.LLVM.Externals
  ) where

import Prelude hiding (void)
import qualified Data.Kind as K
import Eclair.LLVM.Codegen
import Eclair.LLVM.Externals

type AllocCodegenM = StateT Externals ModuleBuilder
type AllocIRCodegenM = IRBuilderT AllocCodegenM

type InitFn
  = Operand -- allocator
  -> AllocIRCodegenM ()
type DestroyFn
  = Operand -- allocator
  -> AllocIRCodegenM ()
type AllocateFn
  =  Operand  -- allocator
  -> Operand  -- length (in bytes)
  -> AllocIRCodegenM Operand
type DeallocateFn
  = Operand   -- allocator
  -> Operand  -- ptr
  -> Operand  -- length
  -> AllocIRCodegenM ()
-- TODO add resize everywhere, just like alloc and free
-- type ResizeFn = Operand  -- ptr
--              -> Operand  -- length
--              -> AllocIRCodegenM ()

-- Helper type that for performing actions on an allocator.
-- This allows a composed allocator to call inner functionality freely.
data VTable
  = VTable
  { vtInit       :: InitFn
  , vtDestroy    :: DestroyFn
  , vtAllocate   :: AllocateFn
  , vtDeallocate :: DeallocateFn
  }

data AllocatorKindTag
  = IsRoot
  | IsNode

data AllocatorKind (k :: AllocatorKindTag) where
  Root :: AllocatorKind 'IsRoot
  Node :: AllocatorKind 'IsNode

type family CreateTypeFn (k :: AllocatorKindTag) where
  CreateTypeFn 'IsRoot = Text -> AllocCodegenM Type
  CreateTypeFn 'IsNode = Text -> Type -> AllocCodegenM Type

type family AllocatorFn (k :: AllocatorKindTag) (ty :: K.Type) where
  AllocatorFn 'IsRoot f = f
  AllocatorFn 'IsNode f = VTable -> f

data None a = None

type family BackingAllocator (k :: AllocatorKindTag) :: (K.Type -> K.Type) where
  BackingAllocator 'IsRoot = None
  BackingAllocator 'IsNode = Allocator

-- First we build up the allocator info in a data-structure.
-- "Stateless" allocators carry no state, and call direct functions provided by the OS (mmap, malloc, ...)
-- "Stateful" allocators do have state, and can further enhance the behavior of the underlying allocator.
-- TODO rename to something else: VTable? Definition? AllocatorFns, rename record fields, update comment
data Allocator repr where
  Allocator
    :: { aType    :: CreateTypeFn k
       , aInit    :: AllocatorFn k InitFn
       , aDestroy :: AllocatorFn k DestroyFn
       , aAlloc   :: AllocatorFn k AllocateFn
       , aFree    :: AllocatorFn k DeallocateFn
       , aInner   :: BackingAllocator k inner
       , aKind    :: AllocatorKind k
       }
    -> Allocator repr

-- Helper type for keeping references to the generated allocator code
-- TODO rename to Allocator
data Alloc
  = Alloc
  { allocTy :: Type
  , allocInitFn :: Operand
  , allocDestroyFn :: Operand
  , allocAllocFn :: Operand
  , allocFreeFn :: Operand
  }

-- Helper type during codegen process.
data Gen
  = Gen
  { generateTy      :: Text -> AllocCodegenM Type
  , generateInit    :: InitFn
  , generateDestroy :: DestroyFn
  , generateAlloc   :: AllocateFn
  , generateFree    :: DeallocateFn
  }

-- This function does the actual code generation of the allocator.
cgAlloc :: Text -> Allocator inner -> AllocCodegenM Alloc
cgAlloc prefix allocator = do
  let g = cgHelper allocator
  allocatorTy <- generateTy g prefix
  allocFn <- function (Name $ prefix <> "_alloc")
                [(ptr allocatorTy, "allocator"), (i32, "size")] (ptr void)
                $ \[alloc, size] -> do
    ret =<< generateAlloc g alloc size
  freeFn <- function (Name $ prefix <> "_free")
                [(ptr allocatorTy, "allocator"), (i32, "size")] void
                $ \[alloc, memory, size] -> do
    generateFree g alloc memory size
  initFn <- function (Name $ prefix <> "_init")
                [(ptr allocatorTy, "allocator")] void
                $ \[alloc] -> do
    generateInit g alloc
  destroyFn <- function (Name $ prefix <> "_destroy")
                [(ptr allocatorTy, "allocator")] void
                $ \[alloc] -> do
    generateDestroy g alloc

  pure $ Alloc allocatorTy initFn destroyFn allocFn freeFn
  where
    -- Recursively generates the code
    cgHelper :: Allocator repr -> Gen
    cgHelper = \case
      Allocator genTy genInit genDestroy genAlloc genFree innerAlloc kind ->
        case kind of
          Root ->
            Gen { generateTy = genTy
                , generateInit = genInit
                , generateDestroy = genDestroy
                , generateAlloc = genAlloc
                , generateFree = genFree
                }

          Node ->
            let generated = cgHelper innerAlloc
                vtable = VTable
                  { vtInit = generateInit generated
                  , vtDestroy = generateDestroy generated
                  , vtAllocate = generateAlloc generated
                  , vtDeallocate = generateFree generated
                  }
            in Gen
                { generateTy = \namePrefix -> do
                    -- First generate inner type, then outer type.
                    ty <- generateTy generated namePrefix
                    genTy namePrefix ty
                  -- Pass generated inner code as function, then generate outer code based on that.
                , generateInit = genInit vtable
                , generateDestroy = genDestroy vtable
                , generateAlloc   = genAlloc vtable
                , generateFree    = genFree vtable
                }

-- TODO helper function for root allocators
