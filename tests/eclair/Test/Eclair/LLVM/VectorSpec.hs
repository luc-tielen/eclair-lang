{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.VectorSpec
  ( module Test.Eclair.LLVM.VectorSpec
  ) where

import Prelude hiding (void)
import qualified LLVM.C.API as LibLLVM
import Eclair.LLVM.Vector
import Eclair.LLVM.Codegen hiding (retVoid, nullPtr)
import Eclair.LLVM.Externals
import Foreign.LibFFI
import Foreign hiding (void)
import System.Posix.DynamicLinker
import Control.Exception
import System.Directory.Extra
import System.Process.Extra
import System.FilePath
import Test.Hspec

type Value = Int

data Bindings
  = Bindings
  { dynamicLib :: DL
  , withVec :: (Ptr Vector -> IO ()) -> IO ()
  , bInit :: Ptr Vector -> IO ()
  , bDestroy :: Ptr Vector -> IO ()
  , bPush :: Ptr Vector -> Value -> IO Word32
  , bSize :: Ptr Vector -> IO Word64
  , bCapacity :: Ptr Vector -> IO Word64
  , bGetValue :: Ptr Vector -> Int -> IO Value
  }

spec :: Spec
spec = describe "Vector" $ aroundAll (setupAndTeardown testDir) $ parallel $ do
  it "can be initialized and destroyed" $ \bindings ->
    withVec bindings $ \v -> do
      bInit bindings v
      bDestroy bindings v

  it "can store multiple elements" $ \bindings -> do
    withVec bindings $ \v -> do
      bInit bindings v
      idx1 <- bPush bindings v 42
      idx2 <- bPush bindings v 123
      value2 <- bGetValue bindings v 1
      value1 <- bGetValue bindings v 0
      bDestroy bindings v
      idx1 `shouldBe` 0
      idx2 `shouldBe` 1
      value1 `shouldBe` 42
      value2 `shouldBe` 123

  it "can store duplicate values" $ \bindings -> do
    withVec bindings $ \v -> do
      bInit bindings v
      idx1 <- bPush bindings v 42
      idx2 <- bPush bindings v 42
      value1 <- bGetValue bindings v 0
      value2 <- bGetValue bindings v 1
      bDestroy bindings v
      idx1 `shouldBe` 0
      idx2 `shouldBe` 1
      value1 `shouldBe` 42
      value2 `shouldBe` 42

  it "keeps track of the number of elements inside" $ \bindings ->
    withVec bindings $ \v -> do
      bInit bindings v

      bSize bindings v >>= (`shouldBe` 0)
      -- This vector allocates on initialization
      bCapacity bindings v >>= (`shouldBe` 16)

      _ <- bPush bindings v 1
      bSize bindings v >>= (`shouldBe` 1)
      bCapacity bindings v >>= (`shouldBe` 16)
      _ <- bPush bindings v 2
      bSize bindings v >>= (`shouldBe` 2)
      bCapacity bindings v >>= (`shouldBe` 16)

      for_ [0..13] $ bPush bindings v
      bSize bindings v >>= (`shouldBe` 16)
      bCapacity bindings v >>= (`shouldBe` 16)

      _ <- bPush bindings v 42
      bSize bindings v >>= (`shouldBe` 17)
      bCapacity bindings v >>= (`shouldBe` 32)

      for_ [0..15] $ bPush bindings v
      bSize bindings v >>= (`shouldBe` 33)
      bCapacity bindings v >>= (`shouldBe` 64)

      bDestroy bindings v

  it "always keeps order of elements, even after resizing" $ \bindings ->
    withVec bindings $ \v -> do
      bInit bindings v

      -- This does several reallocations
      for_ [0..99] $ bPush bindings v
      bSize bindings v >>= (`shouldBe` 100)
      bCapacity bindings v >>= (`shouldBe` 128)

      for_ [0..99] $ \i -> do
        bGetValue bindings v i >>= (`shouldBe` i)

      bDestroy bindings v

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
  -> (Vector -> Operand -> Operand -> ModuleBuilderT IO ())
  -> FilePath -> IO ()
compileCode cgExts cgHelperCode dir = do
  ctx <- LibLLVM.mkContext
  llvmMod <- LibLLVM.mkModule ctx "eclair"
  td <- LibLLVM.getTargetData llvmMod
  llvmIR <- runModuleBuilderT $ do
    exts <- cgExts
    let cfg = Config Nothing ctx td
    vec <- instantiate "test" i32 $ runConfigT cfg $
      codegen exts Nothing -- TODO destructor
    cgHelperCode vec (extMalloc exts) (extFree exts)
  let llvmIRText = ppllvm llvmIR
  writeFileText (llFile dir) llvmIRText
  callProcess "clang" ["-fPIC", "-shared", "-O0", "-o", soFile dir, llFile dir]

cgExternals :: ModuleBuilderT IO Externals
cgExternals = do
  mallocFn <- extern "malloc" [i32] (ptr i8)
  freeFn <- extern "free" [ptr i8] void
  memcpyFn <- extern "llvm.memcpy.p0i8.p0i8.i64" [ptr i8, ptr i8, i64, i1] void
  pure $ Externals mallocFn freeFn notUsed memcpyFn notUsed notUsed notUsed

cgTestCode :: Vector -> Operand -> Operand -> ModuleBuilderT IO ()
cgTestCode vec mallocFn freeFn = do
  let vecTypes = vectorTypes vec
      vecTy = tyVector vecTypes
      valueTy = tyElement vecTypes
  _ <- function "eclair_vector_new_test" [] (ptr vecTy) $ \[] ->
    ret =<< call mallocFn [int32 24]
  _ <- function "eclair_vector_delete_test" [(ptr vecTy, "vec")] void $ \[v] ->
    call freeFn [v]
  _ <- function "eclair_vector_capacity_test" [(ptr vecTy, "vec")] i32 $ \[v] -> do
    capPtr <- gep v [int32 0, int32 2]
    ret =<< load capPtr 0
  _ <- function "eclair_value_new_test" [(i32, "value")] (ptr valueTy) $ \[v] -> do
    vPtr <- call mallocFn [int32 4]
    store vPtr 0 v
    ret vPtr
  _ <- function "eclair_value_delete_test" [(ptr valueTy, "value")] void $ \[v] ->
    call freeFn [v]
  pass

loadNativeCode :: FilePath -> IO Bindings
loadNativeCode dir = do
  lib <- dlopen (soFile dir) [RTLD_LAZY]
  fnNew <- dlsym lib "eclair_vector_new_test"
  fnDelete <- dlsym lib "eclair_vector_delete_test"
  fnValueNew <- dlsym lib "eclair_value_new_test"
  fnValueDelete <- dlsym lib "eclair_value_delete_test"
  fnInit <- dlsym lib "eclair_vector_init_test"
  fnDestroy <- dlsym lib "eclair_vector_destroy_test"
  fnPush <- dlsym lib "eclair_vector_push_test"
  fnSize <- dlsym lib "eclair_vector_size_test"
  fnCapacity <- dlsym lib "eclair_vector_capacity_test"
  fnGetValue <- dlsym lib "eclair_vector_get_value_test"
  pure $ Bindings
    { dynamicLib = lib
    , withVec = mkWithVec fnNew fnDelete
    , bInit = mkInit fnInit
    , bDestroy = mkDestroy fnDestroy
    , bPush = mkPush fnValueNew fnValueDelete fnPush
    , bSize = mkSize fnSize
    , bCapacity = mkCapacity fnCapacity
    , bGetValue = mkGetValue fnGetValue
    }
  where
    mkNew fn = callFFI fn (retPtr retVoid) []
    mkDelete fn vec = callFFI fn retVoid [argPtr vec]
    mkWithVec fnNew fnDelete =
      bracket (castPtr <$> mkNew fnNew) (mkDelete fnDelete)
    mkInit fn vec = callFFI fn retVoid [argPtr vec]
    mkDestroy fn vec = callFFI fn retVoid [argPtr vec]
    mkPush fnValueNew fnValueDelete fn vec value =
      withValue fnValueNew fnValueDelete value $ \valuePtr ->
        fromIntegral <$> callFFI fn retCUInt [argPtr vec, argPtr valuePtr]
    mkSize fn vec =
      fromIntegral <$> callFFI fn retCULong [argPtr vec]
    mkCapacity fn vec =
      fromIntegral <$> callFFI fn retCULong [argPtr vec]
    mkGetValue fn vec idx = do
      resultPtr <- callFFI fn (retPtr retCUInt) [argPtr vec, argCUInt $ fromIntegral idx]
      fromIntegral <$> peek resultPtr
    withValue fnNew fnDelete value =
      bracket
        (castPtr <$> callFFI fnNew (retPtr retCUChar) [argCUInt $ fromIntegral value])
        (\valuePtr -> callFFI fnDelete retVoid [argPtr valuePtr])

testDir :: FilePath
testDir = "/tmp/eclair-vector"

llFile, soFile :: FilePath -> FilePath
llFile dir = dir </> "vector.ll"
soFile dir = dir </> "vector.so"

notUsed :: a
notUsed = panic "Not used"
