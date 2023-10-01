{-# OPTIONS_GHC -Wno-deprecations -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.Allocator.PageSpec
  ( module Test.Eclair.LLVM.Allocator.PageSpec
  ) where

import Prelude hiding (void)
import Control.Monad.Morph
import Eclair.LLVM.Allocator.Page
import Eclair.LLVM.Allocator.Common
import Eclair.LLVM.Codegen hiding (retVoid)
import System.FilePath
import System.Directory.Extra
import System.Process.Extra
import System.Posix.DynamicLinker
import Foreign.LibFFI
import Test.Hspec
import Control.Exception (bracket)
import Foreign.Ptr
import Foreign.C
import Foreign (Storable(peek, poke))


type I8 = CUChar

data PageAllocator

data Bindings
  = Bindings
  { dynamicLib :: DL
  , withAlloc :: (Ptr PageAllocator -> IO ()) -> IO ()
  , fnAlloc :: Ptr PageAllocator -> CSize -> IO (Ptr I8)
  , fnFree :: Ptr PageAllocator -> Ptr I8 -> CSize -> IO ()
  , fnInit :: Ptr PageAllocator -> IO ()
  , fnDestroy :: Ptr PageAllocator -> IO ()
  }

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

setupAndTeardown :: FilePath -> ActionWith Bindings -> IO ()
setupAndTeardown dir =
  bracket (setup dir) teardown

setup :: FilePath -> IO Bindings
setup dir = do
  createDirectoryIfMissing False dir
  compileAllocatorCode dir
  loadNativeCode dir

teardown :: Bindings -> IO ()
teardown (Bindings lib _ _ _ _ _) =
  dlclose lib

compileAllocatorCode :: FilePath -> IO ()
compileAllocatorCode dir = do
  llvmIR <- runModuleBuilderT $ do
    -- mmap [hint, numBytes', prot, flags, noFd, offset]
    mmapFn <- extern "mmap" [ptr i8, i32, i32, i32, i32, i32] (ptr i8)
    -- munmap [memory, len']
    munmapFn <- extern "munmap" [ptr i8, i32] void
    let exts = Externals mmapFn munmapFn notUsed notUsed notUsed notUsed notUsed
        cgBlueprint = flip evalStateT exts $ cgAlloc "PageAllocator" allocator
    Blueprint ty _ _ _ _ <- hoist intoIO cgBlueprint
    -- Helper test code for initializing and freeing a struct from native code:
    _ <- function "PageAllocator_new" [] (ptr ty) $ \[] ->
      ret =<< call mmapFn [Eclair.LLVM.Codegen.nullPtr VoidType, int32 1, int32 2, int32 2, int32 32, int32 (-1), int32 0]
    _ <- function "PageAllocator_delete" [(ptr ty, "allocator")] void $ \[alloc, len] ->
      call munmapFn [alloc, len]
    pass
  let llvmIRText = ppllvm llvmIR
  writeFileText (llFile dir) llvmIRText
  callProcess "clang" ["-fPIC", "-shared", "-O0", "-o", soFile dir, llFile dir]

testDir :: FilePath
testDir = "/tmp/eclair-PageAllocator"

llFile, soFile :: FilePath -> FilePath
llFile dir = dir </> "allocator.ll"
soFile dir = dir </> "allocator.so"

loadNativeCode :: FilePath -> IO Bindings
loadNativeCode dir = do
  lib <- dlopen (soFile dir) [RTLD_LAZY]
  newFn <- dlsym lib "PageAllocator_new"
  deleteFn <- dlsym lib "PageAllocator_delete"
  allocFn <- dlsym lib "PageAllocator_alloc"
  munmapFn <- dlsym lib "PageAllocator_free"
  initFn <- dlsym lib "PageAllocator_init"
  destroyFn <- dlsym lib "PageAllocator_destroy"
  pure $ Bindings
    { dynamicLib = lib
    , withAlloc = mkWithAlloc newFn deleteFn
    , fnAlloc = mkAlloc allocFn
    , fnFree = mkFree munmapFn
    , fnInit = mkInit initFn
    , fnDestroy = mkDestroy destroyFn
    }
  where
    mkAlloc fn pageAllocator numBytes =
      callFFI fn (retPtr retCUChar)
        [ argPtr pageAllocator
        , argCSize $ fromIntegral numBytes
        ]
    mkFree fn pageAllocator memory numBytes =
      callFFI fn retVoid
        [ argPtr pageAllocator
        , argPtr memory
        , argCSize $ fromIntegral numBytes
        ]
    mkInit fn pageAllocator =
      callFFI fn retVoid [argPtr pageAllocator]
    mkDestroy fn pageAllocator =
      callFFI fn retVoid [argPtr pageAllocator]
    mkNew fn =
      callFFI fn (retPtr retVoid) []
    mkDelete fn pageAllocator =
      callFFI fn retVoid [argPtr pageAllocator]
    mkWithAlloc newFn deleteFn =
      bracket (castPtr <$> mkNew newFn) (mkDelete deleteFn)

notUsed :: a
notUsed = undefined

intoIO :: Identity a -> IO a
intoIO = pure . runIdentity
