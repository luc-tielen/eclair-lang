{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.Allocator.MallocSpec
  ( module Test.Eclair.LLVM.Allocator.MallocSpec
  ) where

import Prelude hiding (void)
import Eclair.LLVM.Allocator.Malloc
import Eclair.LLVM.Allocator.Common
import Eclair.LLVM.Codegen hiding (retVoid, nullPtr)
import System.Directory.Extra
import System.Posix.DynamicLinker
import Control.Exception
import Foreign.Ptr
import Foreign hiding (void)
import Test.Eclair.LLVM.Allocator.Utils
import Test.Hspec


data Mallocator

spec :: Spec
spec = describe "Mallocator" $ aroundAll (setupAndTeardown testDir) $ parallel $ do
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
      memory `shouldNotBe` nullPtr
      poke memory value
      value' <- peek memory
      fnFree bindings obj memory numBytes
      fnDestroy bindings obj
      value' `shouldBe` value

setupAndTeardown :: FilePath -> ActionWith (Bindings Mallocator) -> IO ()
setupAndTeardown dir =
  bracket (setup dir) teardown

setup :: FilePath -> IO (Bindings Mallocator)
setup dir = do
  createDirectoryIfMissing False dir
  compileAllocatorCode allocator prefix cgExternals cgTestCode dir
  loadNativeCode prefix dir

teardown :: Bindings Mallocator -> IO ()
teardown =
  dlclose . dynamicLib

cgExternals :: ModuleBuilderT IO Externals
cgExternals = do
  mallocFn <- extern "malloc" [i32] (ptr i8)
  freeFn <- extern "free" [ptr i8] void
  pure $ Externals mallocFn freeFn notUsed notUsed notUsed notUsed notUsed

-- Helper test code for initializing and freeing a struct from native code:
cgTestCode :: Type -> Operand -> Operand -> ModuleBuilderT IO ()
cgTestCode ty mallocFn freeFn = do
  _ <- function "mallocator_new" [] (ptr ty) $ \[] ->
    ret =<< call mallocFn [int32 1]
  _ <- function "mallocator_delete" [(ptr ty, "allocator")] void $ \[alloc] ->
    call freeFn [alloc]
  pass

prefix :: Text
prefix = "mallocator"

testDir :: FilePath
testDir = "/tmp/eclair-mallocator"

notUsed :: a
notUsed = panic "Not used"
