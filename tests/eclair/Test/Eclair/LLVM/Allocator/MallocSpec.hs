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
import Foreign.Ptr (FunPtr)


--     p <- callFFI malloc (retPtr retVoid) [argCSize (2^30)]
--     callFFI memset (retPtr retVoid) [argPtr p, argCInt 0, argCSize (2^30)]
--     callFFI memset (retPtr retVoid) [argPtr nullPtr, argCInt 0, argCSize 1]

data Alloc
  = Alloc DL (FunPtr ()) (FunPtr ()) (FunPtr ()) (FunPtr ()) (FunPtr ()) (FunPtr ())

spec :: Spec
spec = fdescribe "Mallocator" $
  aroundAll (setupAndTeardown testDir) $ do
    parallel $ do
      it "can be initialized and destroyed" $ \alloc -> do
        let Alloc _ newFn deleteFn _ _ initFn destroyFn = alloc
        mallocator <- callFFI newFn (retPtr retVoid) []
        callFFI initFn retVoid [argPtr mallocator]
        callFFI destroyFn retVoid [argPtr mallocator]
        callFFI deleteFn retVoid [argPtr mallocator]

      it "can allocate memory" $ \alloc -> do
        pending

      it "can free memory" $ \alloc -> do
        pending

setupAndTeardown :: FilePath -> ActionWith Alloc -> IO ()
setupAndTeardown dir =
  bracket (setup dir) teardown

setup :: FilePath -> IO Alloc
setup dir = do
  createDirectoryIfMissing False dir
  llvmIR <- runModuleBuilderT $ do
    mallocFn <- extern "malloc" [i32] (ptr i8)
    freeFn <- extern "free" [ptr i8] void
    let exts = Externals mallocFn freeFn notUsed notUsed notUsed notUsed notUsed
        cgBlueprint = flip evalStateT exts $ cgAlloc "mallocator" allocator
    Blueprint ty _ _ _ _ <- hoist intoIO cgBlueprint
    _ <- function "mallocator_new" [] (ptr ty) $ \[] -> do
      ret =<< call mallocFn [int32 1]
    _ <- function "mallocator_delete" [(ptr ty, "allocator")] void $ \[alloc] -> do
      call freeFn [alloc]
    pass
  let llvmIRText = ppllvm llvmIR
  writeFileText (llFile dir) llvmIRText
  callProcess "clang" ["-fPIC", "-shared", "-O0", "-o", soFile dir, llFile dir]
  lib <- dlopen (soFile testDir) [RTLD_LAZY]
  newFn <- dlsym lib "mallocator_new"
  deleteFn <- dlsym lib "mallocator_delete"
  allocFn <- dlsym lib "mallocator_alloc"
  freeFn <- dlsym lib "mallocator_free"
  initFn <- dlsym lib "mallocator_init"
  destroyFn <- dlsym lib "mallocator_destroy"
  pure $ Alloc lib newFn deleteFn allocFn freeFn initFn destroyFn

teardown :: Alloc -> IO ()
teardown (Alloc lib _ _ _ _ _ _) =
  dlclose lib

notUsed :: a
notUsed = undefined

testDir :: FilePath
testDir = "/tmp/eclair-mallocator"

intoIO :: Identity a -> IO a
intoIO = pure . runIdentity

llFile, soFile :: FilePath -> FilePath
llFile dir = dir </> "allocator.ll"
soFile dir = dir </> "allocator.so"
