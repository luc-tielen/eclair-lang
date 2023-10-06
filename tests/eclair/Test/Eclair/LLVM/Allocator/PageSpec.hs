{-# OPTIONS_GHC -Wno-deprecations -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.Allocator.PageSpec
  ( module Test.Eclair.LLVM.Allocator.PageSpec
  ) where

import Prelude hiding (void)
import Eclair.LLVM.Allocator.Page
import Eclair.LLVM.Allocator.Common
import Test.Eclair.LLVM.Allocator.Utils
import Eclair.LLVM.Codegen hiding (retVoid)
import System.Directory.Extra
import System.Posix.DynamicLinker
import Test.Hspec
import Control.Exception (bracket)
import Foreign (Storable(peek, poke))

data PageAllocator

spec :: Spec
spec = describe "PageAllocator" $
  aroundAll (setupAndTeardown testDir) $ parallel $ do
    it "can be initialized and destroyed" $ \bindings ->
      withAlloc bindings $ \obj -> do
        fnInit bindings obj
        fnDestroy bindings obj

    it "can allocate and free memory" $ \bindings -> do
      let numBytes = 1
          value = 42
      withAlloc bindings $ \obj -> do
        fnInit bindings obj
        memory <- fnAlloc bindings obj numBytes
        poke memory value
        value' <- peek memory
        fnFree bindings obj memory numBytes
        fnDestroy bindings obj
        value' `shouldBe` value

setupAndTeardown :: FilePath -> ActionWith (Bindings PageAllocator) -> IO ()
setupAndTeardown dir =
  bracket (setup dir) teardown

setup :: FilePath -> IO (Bindings PageAllocator)
setup dir = do
  createDirectoryIfMissing False dir
  compileAllocatorCode allocator prefix cgExternals cgTestCode dir
  loadNativeCode prefix dir

teardown :: Bindings PageAllocator -> IO ()
teardown =
  dlclose . dynamicLib

cgExternals :: ModuleBuilderT IO Externals
cgExternals = do
  -- mmap [hint, numBytes', prot, flags, noFd, offset]
  mmapFn <- extern "mmap" [ptr i8, i32, i32, i32, i32, i32] (ptr i8)
  -- munmap [memory, len']
  munmapFn <- extern "munmap" [ptr i8, i32] i64
  pure $ Externals mmapFn munmapFn notUsed notUsed notUsed notUsed notUsed

-- Helper test code for initializing and freeing a struct from native code:
cgTestCode :: Type -> Operand -> Operand -> ModuleBuilderT IO ()
cgTestCode ty mmapFn munmapFn = do
  _ <- function "pageallocator_new" [] (ptr ty) $ \[] ->
    ret =<< call mmapFn [nullPtr VoidType, int32 1, int32 2, int32 2, int32 32, int32 (-1), int32 0]
  _ <- function "pageallocator_delete" [(ptr ty, "allocator"), (i64, "len")] i32 $ \[alloc, len] -> 
    call munmapFn [alloc, len]
  pass

prefix :: Text
prefix = "pageallocator"

testDir :: FilePath
testDir = "/tmp/eclair-pageallocator"

notUsed :: a
notUsed = panic "Not used"
