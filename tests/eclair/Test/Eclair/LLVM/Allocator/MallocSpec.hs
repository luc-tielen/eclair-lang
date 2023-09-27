{-# OPTIONS_GHC -Wno-deprecations -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.Allocator.MallocSpec
  ( module Test.Eclair.LLVM.Allocator.MallocSpec
  ) where

import Prelude hiding (void)
import Control.Monad.Morph
import Eclair.LLVM.Allocator.Malloc
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

data Mallocator

data Bindings
  = Bindings
  { dynamicLib :: DL
  , withAlloc :: (Ptr Mallocator -> IO ()) -> IO ()
  , fnAlloc :: Ptr Mallocator -> CSize -> IO (Ptr I8)
  , fnFree :: Ptr Mallocator -> Ptr I8 -> CSize -> IO ()
  , fnInit :: Ptr Mallocator -> IO ()
  , fnDestroy :: Ptr Mallocator -> IO ()
  }

spec :: Spec
spec = describe "Mallocator" $
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
    mallocFn <- extern "malloc" [i32] (ptr i8)
    freeFn <- extern "free" [ptr i8] void
    let exts = Externals mallocFn freeFn notUsed notUsed notUsed notUsed notUsed
        cgBlueprint = flip evalStateT exts $ cgAlloc "mallocator" allocator
    Blueprint ty _ _ _ _ <- hoist intoIO cgBlueprint
    -- Helper test code for initializing and freeing a struct from native code:
    _ <- function "mallocator_new" [] (ptr ty) $ \[] ->
      ret =<< call mallocFn [int32 1]
    _ <- function "mallocator_delete" [(ptr ty, "allocator")] void $ \[alloc] ->
      call freeFn [alloc]
    pass
  let llvmIRText = ppllvm llvmIR
  writeFileText (llFile dir) llvmIRText
  callProcess "clang" ["-fPIC", "-shared", "-O0", "-o", soFile dir, llFile dir]

testDir :: FilePath
testDir = "/tmp/eclair-mallocator"

llFile, soFile :: FilePath -> FilePath
llFile dir = dir </> "allocator.ll"
soFile dir = dir </> "allocator.so"

loadNativeCode :: FilePath -> IO Bindings
loadNativeCode dir = do
  lib <- dlopen (soFile dir) [RTLD_LAZY]
  newFn <- dlsym lib "mallocator_new"
  deleteFn <- dlsym lib "mallocator_delete"
  allocFn <- dlsym lib "mallocator_alloc"
  freeFn <- dlsym lib "mallocator_free"
  initFn <- dlsym lib "mallocator_init"
  destroyFn <- dlsym lib "mallocator_destroy"
  pure $ Bindings
    { dynamicLib = lib
    , withAlloc = mkWithAlloc newFn deleteFn
    , fnAlloc = mkAlloc allocFn
    , fnFree = mkFree freeFn
    , fnInit = mkInit initFn
    , fnDestroy = mkDestroy destroyFn
    }
  where
    mkAlloc fn mallocator numBytes =
      callFFI fn (retPtr retCUChar)
        [ argPtr mallocator
        , argCSize $ fromIntegral numBytes
        ]
    mkFree fn mallocator memory numBytes =
      callFFI fn retVoid
        [ argPtr mallocator
        , argPtr memory
        , argCSize $ fromIntegral numBytes
        ]
    mkInit fn mallocator =
      callFFI fn retVoid [argPtr mallocator]
    mkDestroy fn mallocator =
      callFFI fn retVoid [argPtr mallocator]
    mkNew fn =
      callFFI fn (retPtr retVoid) []
    mkDelete fn mallocator =
      callFFI fn retVoid [argPtr mallocator]
    mkWithAlloc newFn deleteFn =
      bracket (castPtr <$> mkNew newFn) (mkDelete deleteFn)

notUsed :: a
notUsed = undefined

intoIO :: Identity a -> IO a
intoIO = pure . runIdentity
