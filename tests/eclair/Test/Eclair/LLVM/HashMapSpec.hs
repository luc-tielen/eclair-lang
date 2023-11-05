{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.HashMapSpec
  ( module Test.Eclair.LLVM.HashMapSpec
  ) where

import Prelude hiding (void, HashMap, Symbol)
import Control.Exception
import Control.Monad.Morph
import qualified Test.Eclair.LLVM.SymbolUtils as S
import qualified LLVM.C.API as LibLLVM
import Eclair.LLVM.HashMap
import qualified Eclair.LLVM.Symbol as S
import Eclair.LLVM.Codegen hiding (retVoid, nullPtr)
import Eclair.LLVM.Externals
import Foreign.LibFFI
import Foreign hiding (void)
import System.Posix.DynamicLinker
import System.Directory.Extra
import System.Process.Extra
import System.FilePath
import Test.Hspec

type Value = Word32

data Bindings
  = Bindings
  { dynamicLib :: DL
  , symBindings :: S.Bindings
  , withHashMap :: (Ptr HashMap -> IO ()) -> IO ()
  , bInit :: Ptr HashMap -> IO ()
  , bDestroy :: Ptr HashMap -> IO ()
  , bGetOrPut :: Ptr HashMap -> Ptr S.Symbol -> Value -> IO Value
  , bLookup :: Ptr HashMap -> Ptr S.Symbol -> IO Value
  , bContains :: Ptr HashMap -> Ptr S.Symbol -> IO Bool
  }

spec :: Spec
spec = describe "HashMap" $ aroundAll (setupAndTeardown testDir) $ parallel $ do
  it "can be initialized and destroyed" $ \bindings ->
    withHashMap bindings $ \hm -> do
      bInit bindings hm
      bDestroy bindings hm

  it "stores a new value if the requested key was not found" $ \bindings -> do
    let sBindings = symBindings bindings
    withHashMap bindings $ \hm -> do
      bInit bindings hm

      withSym sBindings "abcd" $ \sym -> do
        value1 <- bGetOrPut bindings hm sym 42
        value1 `shouldBe` 42

        -- different symbol -> separate entry in the hashmap
        withSym sBindings "abcdef" $ \sym' -> do
          value3 <- bGetOrPut bindings hm sym' 34
          value3 `shouldBe` 34
          pass

      bDestroy bindings hm

  it "retrieves the old value if the requested key was found" $ \bindings -> do
    let sBindings = symBindings bindings
    withHashMap bindings $ \hm -> do
      bInit bindings hm

      withSym sBindings "abcd" $ \sym -> do
        value1 <- bGetOrPut bindings hm sym 42
        value1 `shouldBe` 42
        value2 <- bGetOrPut bindings hm sym 100
        value2 `shouldBe` 42

        -- same symbol -> same entry in the hashmap
        withSym sBindings "abcd" $ \sym' -> do
          value4 <- bGetOrPut bindings hm sym' 34
          value4 `shouldBe` 42

      bDestroy bindings hm

  it "is possible to lookup keys in the hashmap" $ \bindings -> do
    let sBindings = symBindings bindings
    withHashMap bindings $ \hm -> do
      bInit bindings hm

      -- key found
      withSym sBindings "abcd" $ \sym -> do
        _ <- bGetOrPut bindings hm sym 42
        value <- bLookup bindings hm sym
        value `shouldBe` 42

      -- key not found
      withSym sBindings "123" $ \sym -> do
        value <- bLookup bindings hm sym
        value `shouldBe` 0xffffffff

      bDestroy bindings hm

  it "is possible to check if a hashmap contains a key" $ \bindings -> do
    let sBindings = symBindings bindings
    withHashMap bindings $ \hm -> do
      bInit bindings hm

      -- key found
      withSym sBindings "abcd" $ \sym -> do
        _ <- bGetOrPut bindings hm sym 42
        value <- bContains bindings hm sym
        value `shouldBe` True

      -- key not found
      withSym sBindings "123" $ \sym -> do
        value <- bContains bindings hm sym
        value `shouldBe` False

      bDestroy bindings hm

-- TODO big hashmap test + test for colissions

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
  -> (S.Symbol -> HashMap -> Externals -> ModuleBuilderT IO ())
  -> FilePath -> IO ()
compileCode cgExts cgHelperCode dir = do
  ctx <- LibLLVM.mkContext
  llvmMod <- LibLLVM.mkModule ctx "eclair"
  td <- LibLLVM.getTargetData llvmMod
  llvmIR <- runModuleBuilderT $ do
    exts <- cgExts
    let cfg = Config Nothing ctx td
    sym <- hoist intoIO $ S.codegen exts
    hm <- runConfigT cfg $ codegen sym exts
    cgHelperCode sym hm exts
  let llvmIRText = ppllvm llvmIR
  writeFileText (llFile dir) llvmIRText
  callProcess "clang" ["-fPIC", "-shared", "-O0", "-o", soFile dir, llFile dir]

intoIO :: Identity a -> IO a
intoIO = pure . runIdentity

cgExternals :: ModuleBuilderT IO Externals
cgExternals = do
  mallocFn <- extern "malloc" [i32] (ptr i8)
  freeFn <- extern "free" [ptr i8] void
  memcpyFn <- extern "memcpy" [ptr i8, ptr i8, i64] (ptr i8)
  memcmpFn <- extern "memcmp" [ptr i8, ptr i8, i64] i32
  pure $ Externals mallocFn freeFn notUsed memcpyFn memcmpFn notUsed notUsed

cgTestCode :: S.Symbol -> HashMap -> Externals -> ModuleBuilderT IO ()
cgTestCode sym hm exts = do
  let hmTypes = hashMapTypes hm
      hmTy = tyHashMap hmTypes
      tySym = tyKey hmTypes
      mallocFn = extMalloc exts
      freeFn = extFree exts

  _ <- function "eclair_hashmap_new" [] (ptr hmTy) $ \[] ->
    ret =<< call mallocFn [int32 $ 64 * 32] -- 64 vectors long
  _ <- function "eclair_hashmap_delete" [(ptr hmTy, "hm")] void $ \[h] ->
    call freeFn [h]
  let args = [(ptr hmTy, "hashmap"), (ptr tySym, "symbol")]
  _ <- function "eclair_hashmap_contains_helper" args i8 $ \[h, s] -> do
    result <- call (hashMapContains hm) [h, s]
    ret =<< result `zext` i8

  S.cgTestCode sym exts

loadNativeCode :: FilePath -> IO Bindings
loadNativeCode dir = do
  lib <- dlopen (soFile dir) [RTLD_LAZY]
  sBindings <- S.loadNativeCode' lib
  fnNew <- dlsym lib "eclair_hashmap_new"
  fnDelete <- dlsym lib "eclair_hashmap_delete"
  fnInit <- dlsym lib "eclair_hashmap_init"
  fnDestroy <- dlsym lib "eclair_hashmap_destroy"
  fnGetOrPut <- dlsym lib "eclair_hashmap_get_or_put_value"
  fnContains <- dlsym lib "eclair_hashmap_contains_helper"
  fnLookup <- dlsym lib "eclair_hashmap_lookup"
  pure $ Bindings
    { dynamicLib = lib
    , symBindings = sBindings
    , withHashMap = mkWithHashMap fnNew fnDelete
    , bInit = mkInit fnInit
    , bDestroy = mkDestroy fnDestroy
    , bGetOrPut = mkGetOrPut fnGetOrPut
    , bContains = mkContains fnContains
    , bLookup = mkLookup fnLookup
    }
  where
    mkNew fn = callFFI fn (retPtr retVoid) []
    mkDelete fn hm = callFFI fn retVoid [argPtr hm]
    mkWithHashMap fnNew fnDelete =
      bracket (castPtr <$> mkNew fnNew) (mkDelete fnDelete)
    mkInit fn hm = callFFI fn retVoid [argPtr hm]
    mkDestroy fn hm = callFFI fn retVoid [argPtr hm]
    mkGetOrPut fn hm sym value =
      fromIntegral <$> callFFI fn retCUInt [argPtr hm, argPtr sym, argCUInt $ fromIntegral value]
    mkContains fn hm sym = do
      result <- callFFI fn retCUChar [argPtr hm, argPtr sym]
      pure $ result == 1
    mkLookup fn hm sym =
      fromIntegral <$> callFFI fn retCUInt [argPtr hm, argPtr sym]

testDir :: FilePath
testDir = "/tmp/eclair-hashmap"

llFile, soFile :: FilePath -> FilePath
llFile dir = dir </> "hashmap.ll"
soFile dir = dir </> "hashmap.so"

notUsed :: a
notUsed = panic "Not used"

withSym :: S.Bindings -> String -> (Ptr S.Symbol -> IO a) -> IO a
withSym bindings str f = do
  S.withSymbol bindings $ \sym -> do
    S.bInit bindings sym str
    result <- f sym
    S.bDestroy bindings sym
    pure result
