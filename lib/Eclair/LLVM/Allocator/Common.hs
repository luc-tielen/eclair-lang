{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs, PolyKinds #-}

module Eclair.LLVM.Allocator.Common
  ( Allocator(..)
  , AllocatorKind(..)
  , Alloc(..)
  , Refl(..)  -- TODO use dependency for this?
  , cgAlloc
  , isBaseAlloc
  , AllocCodegenM
  , AllocIRCodegenM
  , module Eclair.LLVM.Externals
  ) where

import Prelude hiding (void)
import Eclair.LLVM.Codegen
import Eclair.LLVM.Externals

type AllocCodegenM = StateT Externals ModuleBuilder
type AllocIRCodegenM = IRBuilderT AllocCodegenM

type AllocFn = Operand  -- length (in bytes)
            -> AllocIRCodegenM Operand
type FreeFn = Operand  -- ptr
           -> Operand  -- length
           -> AllocIRCodegenM ()
-- TODO add resize everywhere, just like alloc and free
-- type ResizeFn = Operand  -- ptr
--              -> Operand  -- length
--              -> AllocIRCodegenM ()

-- Helper type, used as a type level tag to keep track of the shape of an allocator.
data AllocatorKind
  = Base
  | Nested

-- Helper type for type level equality.
data Refl (a :: k) (b :: k) where
  Refl :: Refl a a

-- Helper function, allows us to to check at compile time of k.
-- This helps us properly use the type families in this module.
isBaseAlloc :: Allocator k a -> Maybe (Refl k 'Base)
isBaseAlloc = \case
  Stateless{} -> Just Refl
  Stateful{} -> Nothing

-- Helper type during codegen process.
data Generated (k :: AllocatorKind) where
  GeneratedBase
    :: { baseAlloc :: AllocFn
       , baseFree  :: FreeFn
       }
    -> Generated 'Base
  GeneratedNested
    :: { nestedTy      :: Text -> AllocCodegenM Type
       , nestedInit    :: Operand -> AllocIRCodegenM ()
       , nestedDestroy :: Operand -> AllocIRCodegenM ()
       , nestedAlloc   :: Operand -> AllocFn
       , nestedFree    :: Operand -> FreeFn
       -- TODO add nestedResize :: Operand -> ResizeFn
       }
    -> Generated 'Nested

-- Helper type for keeping references to the generated allocator code
data Alloc (k :: AllocatorKind) where
  BaseAlloc :: { baseAllocFn :: Operand, baseFreeFn :: Operand } -> Alloc 'Base
  NestedAlloc :: { nestedAllocTy :: Type
                 , nestedInitFn :: Operand
                 , nestedDestroyFn :: Operand
                 , nestedAllocFn :: Operand
                 , nestedFreeFn :: Operand
                 }
              -> Alloc 'Nested

type family InnerAllocFn (k :: AllocatorKind) where
  InnerAllocFn 'Base = AllocFn
  InnerAllocFn 'Nested = Operand -> AllocFn

type family InnerFreeFn (k :: AllocatorKind) where
  InnerFreeFn 'Base = FreeFn
  InnerFreeFn 'Nested = Operand -> FreeFn

-- First we build up the allocator info in a data-structure.
-- "Stateless" allocators carry no state, and call direct functions provided by the OS (mmap, malloc, ...)
-- "Stateful" allocators do have state, and can further enhance the behavior of the underlying allocator.
data Allocator (k :: AllocatorKind) repr where
  Stateless
    :: { slAlloc :: AllocFn
       , slFree :: FreeFn
       }
    -> Allocator 'Base repr
  Stateful
    :: { sfType             :: Text -> Maybe Type -> AllocCodegenM Type
       , sfInit             :: (Operand -> AllocIRCodegenM ()) -> Operand -> AllocIRCodegenM ()
       , sfDestroy          :: (Operand -> AllocIRCodegenM ()) -> Operand -> AllocIRCodegenM ()
       , sfAlloc            :: InnerAllocFn k -> Operand -> AllocFn
       , sfFree             :: InnerFreeFn k -> Operand -> FreeFn
       , sfBackingAllocator :: Allocator k inner
       }
    -> Allocator 'Nested (wrapper inner)

-- This function does the actual code generation of the allocator.
cgAlloc :: Text -> Allocator k inner -> AllocCodegenM (Alloc k)
cgAlloc prefix allocator =
  case cgHelper allocator of
    g@GeneratedBase{} -> do
      allocFn <- function (Name $ prefix <> "_alloc")
                    [(i32, "size")] (ptr void)
                    $ \[size] -> do
        baseAlloc g size
      freeFn <- function (Name $ prefix <> "_free")
                    [(ptr void, "memory"), (i32, "size")] void
                    $ \[memory, size] -> do
        baseFree g memory size

      pure $ BaseAlloc allocFn freeFn

    g@GeneratedNested{} -> do
      allocTy <- nestedTy g prefix
      allocFn <- function (Name $ prefix <> "_alloc")
                    [(ptr allocTy, "allocator"), (i32, "size")] (ptr void)
                    $ \[alloc, size] -> do
        nestedAlloc g alloc size
      freeFn <- function (Name $ prefix <> "_free")
                    [(ptr allocTy, "allocator"), (i32, "size")] void
                    $ \[alloc, memory, size] -> do
        nestedFree g alloc memory size
      initFn <- function (Name $ prefix <> "_init")
                    [(ptr allocTy, "allocator")] void
                    $ \[alloc] -> do
        nestedInit g alloc
      destroyFn <- function (Name $ prefix <> "_destroy")
                    [(ptr allocTy, "allocator")] void
                    $ \[alloc] -> do
        nestedDestroy g alloc

      pure $ NestedAlloc allocTy initFn destroyFn allocFn freeFn
  where
    -- Recursively generates the code
    cgHelper :: Allocator k inner -> Generated k
    cgHelper = \case
      alloc@Stateless{} ->
        GeneratedBase { baseAlloc = slAlloc alloc, baseFree = slFree alloc }

      Stateful genTy genInit genDestroy genAlloc genFree innerAlloc ->
        case cgHelper innerAlloc of
          generated@GeneratedBase{} ->
            GeneratedNested
              { nestedTy      = (`genTy` Nothing)
              -- TODO check if init and destroy work correctly
              -- also work with type family here?
              , nestedInit    = genInit (const pass)
              , nestedDestroy = genDestroy (const pass)
              , nestedAlloc   = genAlloc $ baseAlloc generated
              , nestedFree    = genFree $ baseFree generated
              }

          generated@GeneratedNested{} ->
            GeneratedNested
              { nestedTy = \namePrefix -> do
                  -- First generate inner type, then outer type.
                  ty <- nestedTy generated namePrefix
                  genTy namePrefix $ Just ty
                -- Pass generated inner code as function, then generate outer code based on that.
              , nestedInit = genInit $ nestedInit generated
              , nestedDestroy = genDestroy $ nestedDestroy generated
              , nestedAlloc   = genAlloc $ nestedAlloc generated
              , nestedFree    = genFree $ nestedFree generated
              }
