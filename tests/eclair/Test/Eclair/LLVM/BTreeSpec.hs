{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.BTreeSpec
  ( module Test.Eclair.LLVM.BTreeSpec
  ) where

import Prelude hiding (void)
import System.Directory.Extra (createDirectoryIfMissing)
import System.Process.Extra
import System.Posix.DynamicLinker
import System.FilePath
import Eclair.LLVM.BTree
import Eclair.LLVM.Table
import Eclair.LLVM.Externals
import Eclair.LLVM.Codegen hiding (retVoid, nullPtr)
import qualified LLVM.C.API as LibLLVM
import Foreign.Ptr
import Control.Exception
import Test.Hspec
import Foreign.LibFFI


data BTree
data Iter
data Value

data Bindings
  = Bindings
  { dynamicLib :: DL
  , withTree :: (Ptr BTree -> IO ()) -> IO ()
  , withIter :: (Ptr Iter -> IO ()) -> IO ()
  , withValue :: (Ptr Value -> IO ()) -> IO ()
  , bInit :: Ptr BTree -> IO ()
  , bDestroy :: Ptr BTree -> IO ()
  , bPurge :: Ptr BTree -> IO ()
  , bSwap :: Ptr BTree -> Ptr BTree -> IO ()
  , bBegin :: Ptr BTree -> Ptr Iter -> IO ()
  , bEnd :: Ptr BTree -> Ptr Iter -> IO ()
  , bInsert :: Ptr BTree -> Ptr Value -> IO Bool
  , bEmpty :: Ptr BTree -> IO Bool
  , bSize :: Ptr BTree -> IO Word64
  , bContains :: Ptr BTree -> Ptr Value -> IO Bool
  }


spec :: Spec
spec = describe "BTree" $ aroundAll (setupAndTeardown testDir) $ parallel $ do
  it "TODO" $ \btree -> pending
  -- TODO actual tests

setupAndTeardown :: FilePath -> ActionWith Bindings -> IO ()
setupAndTeardown dir =
  bracket (setup dir) teardown

setup :: FilePath -> IO Bindings
setup dir = do
  createDirectoryIfMissing False dir
  let meta = Meta
        { numColumns = 1
        , index = [0]
        , blockSize = 16
        , searchType = Linear
        }
  cgBTree dir meta
  loadNativeCode dir

teardown :: Bindings -> IO ()
teardown =
  dlclose . dynamicLib

cgBTree :: FilePath -> Meta -> IO ()
cgBTree dir meta = do
  ctx <- LibLLVM.mkContext
  llvmMod <- LibLLVM.mkModule ctx "eclair"
  td <- LibLLVM.getTargetData llvmMod
  let cfg = Config Nothing ctx td
  llvmIR <- runModuleBuilderT $ do
    exts <- cgExternals
    table <- instantiate "test" meta $ runConfigT cfg $ codegen exts
    cgHelperCode table (extMalloc exts) (extFree exts)
  let llvmIRText = ppllvm llvmIR
  writeFileText (llFile dir) llvmIRText
  callProcess "clang" ["-fPIC", "-shared", "-O0", "-o", soFile dir, llFile dir]

cgExternals :: ModuleBuilderT IO Externals
cgExternals = do
  mallocFn <- extern "malloc" [i32] (ptr i8)
  freeFn <- extern "free" [ptr i8] void
  pure $ Externals mallocFn freeFn notUsed notUsed notUsed notUsed notUsed

-- Helper test code for initializing and freeing a struct from native code:
cgHelperCode :: Table -> Operand -> Operand -> ModuleBuilderT IO ()
cgHelperCode table mallocFn freeFn = do
  let treeTy = typeObj table
      iterTy = typeIter table
      valueTy = typeValue table
  _ <- function "eclair_btree_new" [] (ptr treeTy) $ \[] ->
    ret =<< call mallocFn [int32 1]
  _ <- function "eclair_btree_delete" [(ptr treeTy, "btree")] void $ \[btree] ->
    call freeFn [btree]
  _ <- function "eclair_iter_new" [] (ptr iterTy) $ \[] ->
    ret =<< call mallocFn [int32 16]
  _ <- function "eclair_iter_delete" [(ptr iterTy, "iter")] void $ \[iter] ->
    call freeFn [iter]
  _ <- function "eclair_value_new" [] (ptr valueTy) $ \[] ->
    ret =<< call mallocFn [int32 4] -- Hardcoded for 1x i32
  _ <- function "eclair_value_delete" [(ptr valueTy, "value")] void $ \[value] ->
    call freeFn [value]
  pass

loadNativeCode :: FilePath -> IO Bindings
loadNativeCode dir = do
  lib <- dlopen (soFile dir) [RTLD_LAZY]
  funcNewTree <- dlsym lib "eclair_btree_new"
  funcDeleteTree <- dlsym lib "eclair_btree_delete"
  funcNewIter <- dlsym lib "eclair_iter_new"
  funcDeleteIter <- dlsym lib "eclair_iter_delete"
  funcNewValue <- dlsym lib "eclair_value_new"
  funcDeleteValue <- dlsym lib "eclair_value_delete"
  funcInit <- dlsym lib "eclair_btree_init_empty"
  funcDestroy <- dlsym lib "eclair_btree_destroy"
  funcPurge <- dlsym lib "eclair_btree_clear"
  funcSwap <- dlsym lib "eclair_btree_swap"
  funcBegin <- dlsym lib "eclair_btree_begin"
  funcEnd <- dlsym lib "eclair_btree_end"
  funcInsert <- dlsym lib "eclair_btree_insert_value"
  funcEmpty <- dlsym lib "eclair_btree_is_empty"
  funcSize <- dlsym lib "eclair_btree_size"
  funcContains <- dlsym lib "eclair_btree_contains"
  pure $ Bindings
    { dynamicLib = lib
    , withTree = mkWithX funcNewTree funcDeleteTree
    , withIter = mkWithX funcNewIter funcDeleteIter
    , withValue = mkWithX funcNewValue funcDeleteValue
    , bInit = mkInit funcInit
    , bDestroy = mkDestroy funcDestroy
    , bPurge = mkPurge funcPurge
    , bSwap = mkSwap funcSwap
    , bBegin = mkBegin funcBegin
    , bEnd = mkEnd funcEnd
    , bInsert = mkInsert funcInsert
    , bEmpty = mkIsEmpty funcEmpty
    , bSize = mkSize funcSize
    , bContains = mkContains funcContains
    }
  where
    mkInit fn tree = callFFI fn retVoid [argPtr tree]
    mkDestroy fn tree = callFFI fn retVoid [argPtr tree]
    mkPurge fn tree = callFFI fn retVoid [argPtr tree]
    mkSwap fn tree1 tree2 = callFFI fn retVoid [argPtr tree1, argPtr tree2]
    mkBegin fn tree resultIter = callFFI fn retVoid [argPtr tree, argPtr resultIter]
    mkEnd fn tree resultIter = callFFI fn retVoid [argPtr tree, argPtr resultIter]
    mkInsert fn tree value = do
      result <- callFFI fn retCUChar [argPtr tree, argPtr value]
      pure $ result == 1
    mkIsEmpty fn tree = do
      result <- callFFI fn retCUChar [argPtr tree]
      pure $ result == 1
    mkSize fn tree = fromIntegral <$> callFFI fn retCULong [argPtr tree]
    mkContains fn tree value = do
      result <- callFFI fn retCUChar [argPtr tree, argPtr value]
      pure $ result == 1
    mkNew fn = callFFI fn (retPtr retVoid) []
    mkDelete fn obj = callFFI fn retVoid [argPtr obj]
    mkWithX newFn deleteFn = bracket (castPtr <$> mkNew newFn) (mkDelete deleteFn)

llFile, soFile :: FilePath -> FilePath
llFile dir = dir </> "btree.ll"
soFile dir = dir </> "btree.so"

testDir :: FilePath
testDir = "/tmp/eclair-btree"

notUsed :: a
notUsed = panic "Not used"
