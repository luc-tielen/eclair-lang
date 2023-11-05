{-# OPTIONS_GHC -Wno-deprecations -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.Allocator.PageSpec
  ( module Test.Eclair.LLVM.Allocator.PageSpec
  ) where

import Prelude hiding (void)
import Control.Monad.Morph
import Eclair.LLVM.Allocator.Page
import Eclair.LLVM.Allocator.Common
import Test.Eclair.LLVM.Allocator.Utils
import Eclair.LLVM.Codegen hiding (retVoid)
import System.Directory.Extra
import System.Posix.DynamicLinker
import Test.Hspec
import Control.Exception (bracket)
import Foreign hiding (void)
import Foreign.LibFFI

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
        let memoryEnd = memory `plusPtr` 4095
        poke memory value
        poke memoryEnd value
        value' <- peek memory
        valueEnd <- peek memoryEnd
        fnFree bindings obj memory numBytes
        fnDestroy bindings obj
        value' `shouldBe` value
        valueEnd `shouldBe` value

    it "rounds up to the nearest page size" $ \_ -> do
      withNearestPageSize $ \roundFn -> do
        result1 <- roundFn 1
        result2 <- roundFn 4096
        result3 <- roundFn 4097
        result4 <- roundFn 0
        result5 <- roundFn 12345678
        result1 `shouldBe` 4096
        result2 `shouldBe` 4096
        result3 `shouldBe` (4096 * 2)
        result4 `shouldBe` 0
        result5 `shouldBe` 12349440

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
  -- Need malloc and free to allocate the allocator itself
  mallocFn <- extern "malloc" [i32] (ptr i8)
  freeFn <- extern "free" [ptr i8] void
  -- mmap [hint, numBytes', prot, flags, noFd, offset]
  mmapFn <- extern "mmap" [ptr void, i64, i32, i32, i32, i32] (ptr void)
  -- munmap [memory, len']
  munmapFn <- extern "munmap" [ptr void, i64] i32
  pure $ Externals mallocFn freeFn notUsed notUsed notUsed mmapFn munmapFn

-- Helper test code for allocating and freeing a struct from native code:
cgTestCode :: Type -> Externals -> ModuleBuilderT IO ()
cgTestCode ty exts = do
  let mallocFn = extMalloc exts
      freeFn = extFree exts
  _ <- function "pageallocator_new" [] (ptr ty) $ \[] ->
    ret =<< call mallocFn [int32 1]
  _ <- function "pageallocator_delete" [(ptr ty, "allocator")] void $ \[alloc] ->
    call freeFn [alloc]
  let roundToNearestInstructions numBytes =
        hoist (hoist intoIO) $ hoist (`evalStateT` exts) $ roundToNearestPageSize numBytes
  _ <- function "nearest_page_size" [(i32, "num_bytes")] i32 $ \[num] ->
    ret =<< roundToNearestInstructions num
  pass

withNearestPageSize :: ((Word32 -> IO Word32) -> IO ()) -> IO ()
withNearestPageSize f =
  bracket open close (\(_, roundFn) -> f roundFn)
  where
    open = do
      dl <- dlopen (soFile testDir) [RTLD_LAZY]
      roundingFn <- dlsym dl "nearest_page_size"
      let roundFn numBytes =
            fromIntegral <$> callFFI roundingFn retCUInt [argCUInt $ fromIntegral numBytes]
      pure (dl, roundFn)
    close = dlclose . fst

prefix :: Text
prefix = "pageallocator"

testDir :: FilePath
testDir = "/tmp/eclair-pageallocator"

notUsed :: a
notUsed = panic "Not used"

intoIO :: Identity a -> IO a
intoIO = pure . runIdentity
