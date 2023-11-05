{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.SymbolSpec
  ( module Test.Eclair.LLVM.SymbolSpec
  ) where

import Prelude hiding (void, Symbol)
import Test.Eclair.LLVM.SymbolUtils
import Control.Monad.Morph
import Control.Exception
import Eclair.LLVM.Symbol
import Eclair.LLVM.Codegen hiding (retVoid, nullPtr)
import Eclair.LLVM.Externals
import System.Posix.DynamicLinker
import System.Directory.Extra
import System.Process.Extra
import Test.Hspec


spec :: Spec
spec = describe "Symbol" $ aroundAll (setupAndTeardown testDir) $ parallel $ do
  it "can be initialized and destroyed" $ \bindings ->
    withSymbol bindings $ \s -> do
      let str = "my example string"
          len = fromIntegral $ length str
      bInit bindings s str
      bLength bindings s >>= (`shouldBe` len)
      bData bindings s >>= (`shouldBe` str)
      bDestroy bindings s

  it "is possible to compare 2 symbols" $ \bindings ->
    withSymbol bindings $ \s1 -> do
      withSymbol bindings $ \s2 -> do
        bInit bindings s1 "abc"
        bInit bindings s2 "1234"

        isEq1 <- bIsEqual bindings s1 s2
        isEq2 <- bIsEqual bindings s1 s1
        isEq3 <- bIsEqual bindings s2 s2

        bDestroy bindings s1
        bDestroy bindings s2

        isEq1 `shouldBe` False
        isEq2 `shouldBe` True
        isEq3 `shouldBe` True


setupAndTeardown :: FilePath -> ActionWith Bindings -> IO ()
setupAndTeardown dir =
  bracket (setup dir) teardown

setup :: FilePath -> IO Bindings
setup dir = do
  createDirectoryIfMissing False dir
  compileCode cgExternals cgTestCode dir
  loadNativeCode dir

teardown :: Bindings -> IO ()
teardown =
  dlclose . dynamicLib

compileCode
  :: ModuleBuilderT IO Externals
  -> (Symbol -> Externals -> ModuleBuilderT IO ())
  -> FilePath -> IO ()
compileCode cgExts cgHelperCode dir = do
  llvmIR <- runModuleBuilderT $ do
    exts <- cgExts
    symbol <- hoist intoIO $ codegen exts
    cgHelperCode symbol exts
  let llvmIRText = ppllvm llvmIR
  writeFileText (llFile dir) llvmIRText
  callProcess "clang" ["-fPIC", "-shared", "-O0", "-o", soFile dir, llFile dir]

cgExternals :: ModuleBuilderT IO Externals
cgExternals = do
  mallocFn <- extern "malloc" [i32] (ptr i8)
  freeFn <- extern "free" [ptr i8] void
  memcpyFn <- extern "memcpy" [ptr i8, ptr i8, i64] (ptr i8)
  memcmpFn <- extern "memcmp" [ptr i8, ptr i8, i64] i32
  pure $ Externals mallocFn freeFn notUsed memcpyFn memcmpFn notUsed notUsed

testDir :: FilePath
testDir = "/tmp/eclair-symbol"

notUsed :: a
notUsed = panic "Not used"

intoIO :: Identity a -> IO a
intoIO = pure . runIdentity
