{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.SymbolSpec
  ( module Test.Eclair.LLVM.SymbolSpec
  ) where

import Prelude hiding (void, Symbol)
import Control.Monad.Morph
import Control.Exception
import Eclair.LLVM.Symbol
import Eclair.LLVM.Codegen hiding (retVoid, nullPtr)
import Eclair.LLVM.Externals
import Foreign.LibFFI
import Foreign hiding (void, bit)
import System.Posix.DynamicLinker
import System.Directory.Extra
import System.Process.Extra
import System.FilePath
import Test.Hspec
import Foreign.C

type I8 = CUChar

data Bindings
  = Bindings
  { dynamicLib :: DL
  , withSymbol :: (Ptr Symbol -> IO ()) -> IO ()
  , bInit :: Ptr Symbol -> String -> IO ()
  , bDestroy :: Ptr Symbol -> IO ()
  , bIsEqual :: Ptr Symbol -> Ptr Symbol -> IO Bool
  , bLength :: Ptr Symbol -> IO Word32
  , bData :: Ptr Symbol -> IO String
  }

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

cgTestCode :: Symbol -> Externals -> ModuleBuilderT IO ()
cgTestCode sym exts = do
  let mallocFn = extMalloc exts
      freeFn = extFree exts
      memcpyFn = extMemcpy exts
      symTy = tySymbol sym
  _ <- function "eclair_symbol_new" [] (ptr symTy) $ \[] ->
    ret =<< call mallocFn [int32 16]
  _ <- function "eclair_symbol_delete" [(ptr symTy, "sym")] void $ \[s] ->
    call freeFn [s]
  let initArgs = [(ptr symTy, "sym"), (i32, "length"), (ptr i8, "data")]
  _ <- function "eclair_symbol_init_helper" initArgs void $ \[s, len, str] -> do
    -- Needed because "str" is freed afterwards
    memory <- call mallocFn [len]
    _ <- call memcpyFn [memory, str, len, bit 0]
    _ <- call (symbolInit sym) [s, len, memory]
    pass
  let isEqArgs = [(ptr symTy, "sym1"), (ptr symTy, "sym2")]
  _ <- function "eclair_symbol_is_equal_helper" isEqArgs i8 $ \[sym1, sym2] -> do
    isEq <- call (symbolIsEqual sym) [sym1, sym2]
    ret =<< isEq `zext` i8
  _ <- function "eclair_symbol_length" [(ptr symTy, "sym")] i32 $ \[s] -> do
    lenPtr <- gep s [int32 0, int32 0]
    ret =<< load lenPtr 0
  _ <- function "eclair_symbol_data" [(ptr symTy, "sym")] (ptr i8) $ \[s] -> do
    lenPtr <- gep s [int32 0, int32 1]
    ret =<< load lenPtr 0
  pass

loadNativeCode :: FilePath -> IO Bindings
loadNativeCode dir = do
  lib <- dlopen (soFile dir) [RTLD_LAZY]
  fnNew <- dlsym lib "eclair_symbol_new"
  fnDelete <- dlsym lib "eclair_symbol_delete"
  fnInit <- dlsym lib "eclair_symbol_init_helper"
  fnDestroy <- dlsym lib "eclair_symbol_destroy"
  fnIsEqual <- dlsym lib "eclair_symbol_is_equal_helper"
  fnLength <- dlsym lib "eclair_symbol_length"
  fnData <- dlsym lib "eclair_symbol_data"
  let getLength = mkLength fnLength
  pure $ Bindings
    { dynamicLib = lib
    , withSymbol = mkWithSymbol fnNew fnDelete
    , bInit = mkInit fnInit
    , bDestroy = mkDestroy fnDestroy
    , bIsEqual = mkIsEqual fnIsEqual
    , bLength = getLength
    , bData = mkData fnData getLength
    }
  where
    mkNew fn = callFFI fn (retPtr retVoid) []
    mkDelete fn sym = callFFI fn retVoid [argPtr sym]
    mkWithSymbol fnNew fnDelete =
      bracket (castPtr <$> mkNew fnNew) (mkDelete fnDelete)
    mkInit fn sym str = do
      let len = fromIntegral $ length str
      callFFI fn retVoid [argPtr sym, argCUInt len, argString str]
    mkDestroy fn sym = callFFI fn retVoid [argPtr sym]
    mkIsEqual fn sym1 sym2 = do
      result <- callFFI fn retCUChar [argPtr sym1, argPtr sym2]
      pure $ result == 1
    mkLength fn sym = do
      fromIntegral <$> callFFI fn retCUInt [argPtr sym]
    mkData fn getLength sym = do
      len <- fromIntegral <$> getLength sym
      strPtr <- callFFI fn (retPtr retCChar) [argPtr sym]
      peekCAStringLen (strPtr, len)

testDir :: FilePath
testDir = "/tmp/eclair-symbol"

llFile, soFile :: FilePath -> FilePath
llFile dir = dir </> "symbol.ll"
soFile dir = dir </> "symbol.so"

notUsed :: a
notUsed = panic "Not used"

intoIO :: Identity a -> IO a
intoIO = pure . runIdentity
