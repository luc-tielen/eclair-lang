{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.BTree.BTreeSpec
  ( module Test.Eclair.LLVM.BTree.BTreeSpec
  ) where

import Prelude hiding (void)
import qualified Relude as R
import System.Directory.Extra
import System.Process.Extra
import System.Posix.DynamicLinker
import System.FilePath
import Control.Exception
import Control.Monad.Morph
import System.Random
import Data.Array.IO hiding (index)
import Foreign.LibFFI
import Foreign hiding (void, newArray)
import Eclair.LLVM.BTree
import Eclair.LLVM.Table
import Eclair.LLVM.Externals
import Eclair.LLVM.Codegen hiding (retVoid, nullPtr)
import qualified LLVM.C.API as LibLLVM
import Test.Hspec


data BTree
data Iter
type Value = Word32

data Bindings
  = Bindings
  { dynamicLib :: DL
  , withTree :: (Ptr BTree -> IO ()) -> IO ()
  , withIter :: forall a. (Ptr Iter -> IO a) -> IO a
  , withValue :: forall a. Value -> (Ptr Value -> IO a) -> IO a
  , bInit :: Ptr BTree -> IO ()
  , bDestroy :: Ptr BTree -> IO ()
  , bPurge :: Ptr BTree -> IO ()
  , bSwap :: Ptr BTree -> Ptr BTree -> IO ()
  , bBegin :: Ptr BTree -> Ptr Iter -> IO ()
  , bEnd :: Ptr BTree -> Ptr Iter -> IO ()
  , bInsert :: Ptr BTree -> Ptr Value -> IO Bool
  , bMerge :: Ptr BTree -> Ptr BTree -> IO ()
  , bEmpty :: Ptr BTree -> IO Bool
  , bSize :: Ptr BTree -> IO Word64
  , bNodeCount :: Ptr BTree -> IO Word64
  , bDepth :: Ptr BTree -> IO Word32
  , bLowerBound :: forall a. Ptr BTree -> Ptr Value -> (Ptr Iter -> IO a) -> IO a
  , bUpperBound :: forall a. Ptr BTree -> Ptr Value -> (Ptr Iter -> IO a) -> IO a
  , bContains :: Ptr BTree -> Ptr Value -> IO Bool
  , bIterCurrent :: Ptr Iter -> IO (Ptr Value)
  , bIterNext :: Ptr Iter -> IO ()
  , bIterIsEqual :: Ptr Iter -> Ptr Iter -> IO Bool
  }


spec :: Spec
spec = describe "BTree" $ aroundAll (setupAndTeardown testDir) $ parallel $ do
  it "can be initialized and destroyed" $ \bindings -> do
    withTree bindings $ \tree -> do
      bInit bindings tree
      bDestroy bindings tree

  it "is possible to remove all elements from the tree" $ \bindings ->
    withTree bindings $ \tree -> do
      bInit bindings tree

      bPurge bindings tree -- empty trees
      empty1 <- bEmpty bindings tree
      bPurge bindings tree -- calling it again
      empty2 <- bEmpty bindings tree

      withValue bindings 1 $ R.void . bInsert bindings tree

      empty3 <- bEmpty bindings tree
      bPurge bindings tree -- non-empty tree
      empty4 <- bEmpty bindings tree
      bPurge bindings tree -- calling it again
      empty5 <- bEmpty bindings tree

      for_ [1..100] $ \i ->
        withValue bindings i $ R.void . bInsert bindings tree
      bPurge bindings tree -- calling it again
      empty6 <- bEmpty bindings tree

      bDestroy bindings tree

      empty1 `shouldBe` True
      empty2 `shouldBe` True
      empty3 `shouldBe` False
      empty4 `shouldBe` True
      empty5 `shouldBe` True
      empty6 `shouldBe` True

  it "should be possible to merge one tree into another" $ \bindings ->
    withTree bindings $ \tree1 -> do
      withTree bindings $ \tree2 -> do
        bInit bindings tree1
        bInit bindings tree2

        for_ [1..4] $ \i -> do
          withValue bindings i $ bInsert bindings tree1
        for_ [2, 4, 6] $ \i -> do
          withValue bindings i $ bInsert bindings tree2

        -- tree1 = "destination", tree2 = "source"
        bMerge bindings tree1 tree2
        list <- treeToList bindings tree1

        bDestroy bindings tree1
        bDestroy bindings tree2

        list `shouldBe` [1, 2, 3, 4, 6]

  it "is possible to swap two trees" $ \bindings -> do
    withTree bindings $ \tree1 -> do
      withTree bindings $ \tree2 -> do
        bInit bindings tree1
        bInit bindings tree2

        for_ [1..100] $ \i -> do
          withValue bindings i $ \value -> do
            _ <- bInsert bindings tree1 value
            pass
          withValue bindings (i + 100) $ \value -> do
            _ <- bInsert bindings tree2 value
            pass

        c1 <- withValue bindings 42 $ bContains bindings tree1
        c2 <- withValue bindings 78 $ bContains bindings tree1
        c3 <- withValue bindings 142 $ bContains bindings tree2
        c4 <- withValue bindings 178 $ bContains bindings tree2

        bSwap bindings tree1 tree2

        c5 <- withValue bindings 42 $ bContains bindings tree2
        c6 <- withValue bindings 78 $ bContains bindings tree2
        c7 <- withValue bindings 142 $ bContains bindings tree1
        c8 <- withValue bindings 178 $ bContains bindings tree1

        bDestroy bindings tree1
        bDestroy bindings tree2

        let result = R.and [c1, c2, c3, c4, c5, c6, c7, c8]
        result `shouldBe` True

  it "is possible to get begin and end iterators" $ \bindings ->
    withTree bindings $ \tree -> do
      withIters bindings $ \beginIter endIter -> do
        bInit bindings tree
        bBegin bindings tree beginIter
        bEnd bindings tree endIter
        beginIter `shouldNotBe` nullPtr
        endIter `shouldNotBe` nullPtr
        bDestroy bindings tree

  it "is possible to iterate over the tree" $ \bindings ->
    withTree bindings $ \tree -> do
      bInit bindings tree

      withValue bindings 4 $ R.void . bInsert bindings tree
      withValue bindings 2 $ R.void . bInsert bindings tree
      withValue bindings 5 $ R.void . bInsert bindings tree
      withValue bindings 1 $ R.void . bInsert bindings tree
      withValue bindings 3 $ R.void . bInsert bindings tree

      withIters bindings $ \beginIter endIter -> do
        bBegin bindings tree beginIter
        bEnd bindings tree endIter
        isEqual <- bIterIsEqual bindings beginIter endIter
        isEqual `shouldBe` False

      values <- treeToList bindings tree

      bDestroy bindings tree
      values `shouldBe` [1, 2, 3, 4, 5]

  it "should have equal begin and end iterators if tree is empty" $ \bindings ->
    withTree bindings $ \tree -> do
      withIters bindings $ \beginIter endIter -> do
        bInit bindings tree
        bBegin bindings tree beginIter
        bEnd bindings tree endIter
        isEqual <- bIterIsEqual bindings beginIter endIter
        isEqual `shouldBe` True
        bDestroy bindings tree

  it "is possible to insert a value" $ \bindings ->
    withTree bindings $ \tree -> do
      withValue bindings 1 $ \value -> do
        bInit bindings tree
        didInsert <- bInsert bindings tree value
        didInsert' <- bInsert bindings tree value
        didInsert `shouldBe` True
        didInsert' `shouldBe` False
        bDestroy bindings tree

  it "is possible to check if the tree is empty" $ \bindings ->
    withTree bindings $ \tree -> do
      bInit bindings tree

      empty1 <- bEmpty bindings tree
      withValue bindings 1 $ R.void . bInsert bindings tree
      empty2 <- bEmpty bindings tree
      withValue bindings 2 $ R.void . bInsert bindings tree
      empty3 <- bEmpty bindings tree

      bDestroy bindings tree

      empty1 `shouldBe` True
      empty2 `shouldBe` False
      empty3 `shouldBe` False

  it "is possible to lookup the size of the tree" $ \bindings ->
    withTree bindings $ \tree -> do
      bInit bindings tree
      size1 <- bSize bindings tree
      R.void $ withValue bindings 1 $ bInsert bindings tree
      size2 <- bSize bindings tree
      for_ [2..100] $ \i -> do
        withValue bindings i $ bInsert bindings tree
      size3 <- bSize bindings tree
      bDestroy bindings tree
      size1 `shouldBe` 0
      size2 `shouldBe` 1
      size3 `shouldBe` 100

  it "is possible to check if the tree contains a certain value" $ \bindings ->
    withTree bindings $ \tree -> do
      bInit bindings tree

      c1 <- withValue bindings 1000 $ bContains bindings tree
      R.void $ withValue bindings 1000 $ bInsert bindings tree
      c2 <- withValue bindings 1000 $ bContains bindings tree

      for_ [1..100] $ \i ->
        withValue bindings i $ \value -> do
          _ <- bInsert bindings tree value
          pass

      c3 <- withValue bindings 42 $ bContains bindings tree
      c4 <- withValue bindings 78 $ bContains bindings tree
      c5 <- withValue bindings 132 $ bContains bindings tree

      c1 `shouldBe` False
      c2 `shouldBe` True
      c3 `shouldBe` True
      c4 `shouldBe` True
      c5 `shouldBe` False

      bDestroy bindings tree

  -- Tests below are taken from Souffle's test suite

  it "should support basic operations on the btree" $ \bindings ->
    withTree bindings $ \tree -> do
      bInit bindings tree

      -- check initial conditions
      bSize bindings tree >>= (`shouldBe` 0)
      bNodeCount bindings tree >>= (`shouldBe` 0)
      bDepth bindings tree >>= (`shouldBe` 0)
      withValue bindings 10 (bContains bindings tree) >>= (`shouldBe` False)
      withValue bindings 12 (bContains bindings tree) >>= (`shouldBe` False)
      withValue bindings 14 (bContains bindings tree) >>= (`shouldBe` False)

      -- add an element

      R.void $ withValue bindings 12 (bInsert bindings tree)
      bSize bindings tree >>= (`shouldBe` 1)
      bNodeCount bindings tree >>= (`shouldBe` 1)
      bDepth bindings tree >>= (`shouldBe` 1)
      withValue bindings 10 (bContains bindings tree) >>= (`shouldBe` False)
      withValue bindings 12 (bContains bindings tree) >>= (`shouldBe` True) -- TODO failing
      withValue bindings 14 (bContains bindings tree) >>= (`shouldBe` False)

      -- add a larger element
      R.void $ withValue bindings 14 (bInsert bindings tree)
      bSize bindings tree >>= (`shouldBe` 2)
      bNodeCount bindings tree >>= (`shouldBe` 1)
      bDepth bindings tree >>= (`shouldBe` 1)
      withValue bindings 10 (bContains bindings tree) >>= (`shouldBe` False)
      withValue bindings 12 (bContains bindings tree) >>= (`shouldBe` True)
      withValue bindings 14 (bContains bindings tree) >>= (`shouldBe` True)

      -- add a smaller element
      R.void $ withValue bindings 10 (bInsert bindings tree)
      bSize bindings tree >>= (`shouldBe` 3)
      bNodeCount bindings tree >>= (`shouldBe` 1)
      bDepth bindings tree >>= (`shouldBe` 1)
      withValue bindings 10 (bContains bindings tree) >>= (`shouldBe` True)
      withValue bindings 12 (bContains bindings tree) >>= (`shouldBe` True)
      withValue bindings 14 (bContains bindings tree) >>= (`shouldBe` True)

      -- cause a split
      R.void $ withValue bindings 11 (bInsert bindings tree)
      bSize bindings tree >>= (`shouldBe` 4)
      withValue bindings 10 (bContains bindings tree) >>= (`shouldBe` True)
      withValue bindings 11 (bContains bindings tree) >>= (`shouldBe` True)
      withValue bindings 12 (bContains bindings tree) >>= (`shouldBe` True)
      withValue bindings 14 (bContains bindings tree) >>= (`shouldBe` True)

      -- adding duplicates
      R.void $ withValue bindings 12 (bInsert bindings tree)
      bSize bindings tree >>= (`shouldBe` 4)
      R.void $ withValue bindings 12 (bInsert bindings tree)
      bSize bindings tree >>= (`shouldBe` 4)
      R.void $ withValue bindings 10 (bInsert bindings tree)
      bSize bindings tree >>= (`shouldBe` 4)

      R.void $ withValue bindings 15 (bInsert bindings tree)
      bSize bindings tree >>= (`shouldBe` 5)
      bNodeCount bindings tree >>= (`shouldBe` 3)
      bDepth bindings tree >>= (`shouldBe` 2)

      R.void $ withValue bindings 16 (bInsert bindings tree)
      bSize bindings tree >>= (`shouldBe` 6)

      bDestroy bindings tree

  it "should automatically remove duplicates" $ \bindings ->
    withTree bindings $ \tree -> do
      bInit bindings tree

      replicateM_ 10 $ withValue bindings 0 $ bInsert bindings tree

      size <- bSize bindings tree
      value <- withIter bindings $ \iter -> do
        bBegin bindings tree iter
        valuePtr <- bIterCurrent bindings iter
        peek valuePtr

      bDestroy bindings tree

      size `shouldBe` 1
      value `shouldBe` 0

  it "should contain the value after it is inserted" $ \bindings ->
    withTree bindings $ \tree -> do
      bInit bindings tree

      let n = 100
      for_ [0..n] $ \i -> do
        R.void $ withValue bindings i $ bInsert bindings tree

        for_ [0..n] $ \j -> do
          contains <- withValue bindings j $ bContains bindings tree
          contains `shouldBe` (j <= i)

      bDestroy bindings tree

  it "should contain the value after it is inserted (reverse)" $ \bindings ->
    withTree bindings $ \tree -> do
      bInit bindings tree

      let n = 100
      for_ [n, (n - 1) .. 0] $ \i -> do
        R.void $ withValue bindings i (bInsert bindings tree)

        for_ [0..n] $ \j -> do
          contains <- withValue bindings j (bContains bindings tree)
          contains `shouldBe` (j >= i)

      bDestroy bindings tree

  it "should contain the value after is inserted (shuffled)" $ \bindings -> do
    let list = [1..10000]
    shuffled <- shuffle list

    withTree bindings $ \tree -> do
      bInit bindings tree

      for_ shuffled $ \i -> do
        R.void $ withValue bindings i (bInsert bindings tree)

      for_ list $ \j -> do
        contains <- withValue bindings j (bContains bindings tree)
        contains `shouldBe` True

      bDestroy bindings tree

  it "should withstand iterator stress test" $ \bindings -> do
    let isSorted xs = sort xs == xs
        list = [1..300]  -- for faster unit tests
        -- list = [1..1000]  -- for real stress test
    shuffled <- shuffle list

    withTree bindings $ \tree -> do
      bInit bindings tree

      for_ shuffled $ \i -> do
        values <- treeToList bindings tree
        -- this is the main check if iterators are working correctly:
        isSorted values `shouldBe` True
        R.void $ withValue bindings i (bInsert bindings tree)

      bDestroy bindings tree

  it "should calculate correct lower and upper bounds of a value" $ \bindings ->
    withTree bindings $ \tree -> do
      let getBound f = flip (f bindings tree) (peek <=< bIterCurrent bindings)
          getLB = getBound bLowerBound
          getUB = getBound bUpperBound

      bInit bindings tree

      for_ [0..10] $ \i -> do
        R.void $ withValue bindings i (bInsert bindings tree)

      lb1 <- withValue bindings 5 getLB
      ub1 <- withValue bindings 5 getUB
      lb1 `shouldBe` 5
      ub1 `shouldBe` 6

      -- add duplicates and check again
      replicateM_ 3 $ R.void $ withValue bindings 5 $ bInsert bindings tree

      lb2 <- withValue bindings 5 getLB
      ub2 <- withValue bindings 5 getUB
      lb2 `shouldBe` 5
      ub2 `shouldBe` 6

      bDestroy bindings tree

  it "should calculate correct lower and upper bound for empty trees" $ \bindings ->
    withTree bindings $ \tree ->
      withIter bindings $ \endIter -> do
        bInit bindings tree
        bEnd bindings tree endIter

        -- empty
        lbIsEnd1 <- withValue bindings 5 $ flip (bLowerBound bindings tree) $
          bIterIsEqual bindings endIter
        ubIsEnd1 <- withValue bindings 5 $ flip (bUpperBound bindings tree) $
          bIterIsEqual bindings endIter
        lbIsEnd1 `shouldBe` True
        ubIsEnd1 `shouldBe` True

        let checkBounds expected3 expected5 = do
              withValue bindings 3 $ flip (bLowerBound bindings tree) $ \lbIter ->
                withValue bindings 3 $ flip (bUpperBound bindings tree) $ \ubIter -> do
                  isEqual <- bIterIsEqual bindings lbIter ubIter
                  isEqual `shouldBe` expected3
              withValue bindings 5 $ flip (bLowerBound bindings tree) $ \lbIter ->
                withValue bindings 5 $ flip (bUpperBound bindings tree) $ \ubIter -> do
                  isEqual <- bIterIsEqual bindings lbIter ubIter
                  isEqual `shouldBe` expected5

        -- insert 4
        R.void $ withValue bindings 4 (bInsert bindings tree)
        checkBounds True True
        -- insert 6
        R.void $ withValue bindings 6 (bInsert bindings tree)
        checkBounds True True
        -- insert 5
        R.void $ withValue bindings 5 (bInsert bindings tree)
        checkBounds True False

        bDestroy bindings tree

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
    let iterParams = IteratorParams
          { ipIterCurrent = fnIterCurrent table
          , ipIterNext = fnIterNext table
          , ipIterIsEqual = fnIterIsEqual table
          , ipTypeIter = typeIter table
          }
    R.void $ hoist intoIO $ instantiate "test" iterParams $
      fnInsertRangeTemplate table
    cgHelperCode table (extMalloc exts) (extFree exts)
  let llvmIRText = ppllvm llvmIR
  writeFileText (llFile dir) llvmIRText
  -- Next line is a hack, because we can't access node types from the test:
  appendFileText (llFile dir) helperCodeAppendix
  callProcess "clang" ["-fPIC", "-shared", "-O0", "-o", soFile dir, llFile dir]

cgExternals :: ModuleBuilderT IO Externals
cgExternals = do
  mallocFn <- extern "malloc" [i32] (ptr i8)
  freeFn <- extern "free" [ptr i8] void
  memsetFn <- extern "llvm.memset.p0i8.i64" [ptr i8, i8, i64, i1] void
  pure $ Externals mallocFn freeFn memsetFn notUsed notUsed notUsed notUsed

-- Helper test code for initializing and freeing a struct from native code:
cgHelperCode :: Monad m => Table -> Operand -> Operand -> ModuleBuilderT m ()
cgHelperCode table mallocFn freeFn = do
  let treeTy = typeObj table
      iterTy = typeIter table
      valueTy = typeValue table
  _ <- function "eclair_btree_new" [] (ptr treeTy) $ \[] ->
    ret =<< call mallocFn [int32 16]
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
  -- Next function is needed because returning i1 is not C ABI compatible
  _ <- function "eclair_btree_contains_helper_test" [(ptr treeTy, "tree"), (ptr valueTy, "val")] i8 $ \[tree, val] -> do
    result <- call (fnContains table) [tree, val] >>= (`zext` i8)
    ret result
  pass

helperCodeAppendix :: Text
helperCodeAppendix = unlines
  [ ""
  , "define external ccc i64 @node_count(ptr %node_0) {"
  , "start:"
  , "  %stack.ptr_0 = alloca i64"  -- count
  , "  store i64 1, ptr %stack.ptr_0"
  , "  %0 = getelementptr %node_t_test, ptr %node_0, i32 0, i32 0, i32 3"
  , "  %1 = load i1, ptr %0" -- node type
  , "  %2 = icmp eq i1 %1, 0" -- is leaf?
  , "  br i1 %2, label %if_0, label %end_if_0"
  , "if_0:"
  , "  ret i64 1"
  , "end_if_0:"
  , "  %3 = getelementptr %node_t_test, ptr %node_0, i32 0, i32 0, i32 2"
  , "  %4 = load i16, ptr %3"
  , "  br label %for_begin_0"
  , "for_begin_0:"
  , "  %5 = phi i16 [0, %end_if_0], [%12, %for_body_0]"
  , "  %6 = icmp ule i16 %5, %4"
  , "  br i1 %6, label %for_body_0, label %for_end_0"
  , "for_body_0:"
  , "  %7 = load i64, ptr %stack.ptr_0" -- count
  , "  %8 = getelementptr %inner_node_t_test, ptr %node_0, i32 0, i32 1, i16 %5" -- child ptr
  , "  %9 = load ptr, ptr %8" -- child
  , "  %10 = call ccc i64 @node_count(ptr %9)"
  , "  %11 = add i64 %7, %10"
  , "  store i64 %11, ptr %stack.ptr_0"
  , "  %12 = add i16 1, %5"
  , "  br label %for_begin_0"
  , "for_end_0:"
  , "  %13 = load i64, ptr %stack.ptr_0"
  , "  ret i64 %13"
  , "}"
  , ""
  , "define external ccc i64 @eclair_btree_node_count_test(ptr %tree_0) {"
  , "start:"
  , "  %0 = getelementptr %btree_t_test, ptr %tree_0, i32 0, i32 0"
  , "  %1 = load ptr, ptr %0"
  , "  %2 = icmp eq ptr %1, zeroinitializer"
  , "  br i1 %2, label %null_0, label %not_null_0"
  , "null_0:"
  , "  ret i64 0"
  , "not_null_0:"
  , "  %3 = call ccc i64 @node_count(ptr %1)"
  , "  ret i64 %3"
  , "}"
  , ""
  , "define external ccc i32 @node_depth(ptr %node_0) {"
  , "start:"
  , "  %0 = getelementptr %node_t_test, ptr %node_0, i32 0, i32 0, i32 3"
  , "  %1 = load i1, ptr %0" -- node type
  , "  %2 = icmp eq i1 %1, 0" -- is leaf?
  , "  br i1 %2, label %if_0, label %end_if_0"
  , "if_0:"
  , "  ret i32 1"
  , "end_if_0:"
  , "  %3 = getelementptr %inner_node_t_test, ptr %node_0, i32 0, i32 1, i16 0" -- child ptr
  , "  %4 = load ptr, ptr %3" -- child
  , "  %5 = call ccc i32 @node_depth(ptr %4)"
  , "  %6 = add i32 %5, 1"
  , "  ret i32 %6"
  , "}"
  , ""
  , "define external ccc i32 @eclair_btree_depth_test(ptr %tree_0) {"
  , "start:"
  , "  %0 = getelementptr %btree_t_test, ptr %tree_0, i32 0, i32 0"
  , "  %1 = load ptr, ptr %0"
  , "  %2 = icmp eq ptr %1, zeroinitializer"
  , "  br i1 %2, label %null_0, label %not_null_0"
  , "null_0:"
  , "  ret i32 0"
  , "not_null_0:"
  , "  %3 = call ccc i32 @node_depth(ptr %1)"
  , "  ret i32 %3"
  , "}"
  ]

loadNativeCode :: FilePath -> IO Bindings
loadNativeCode dir = do
  lib <- dlopen (soFile dir) [RTLD_LAZY]
  funcNewTree <- dlsym lib "eclair_btree_new"
  funcDeleteTree <- dlsym lib "eclair_btree_delete"
  funcNewIter <- dlsym lib "eclair_iter_new"
  funcDeleteIter <- dlsym lib "eclair_iter_delete"
  funcNewValue <- dlsym lib "eclair_value_new"
  funcDeleteValue <- dlsym lib "eclair_value_delete"
  funcInit <- dlsym lib "eclair_btree_init_empty_test"
  funcDestroy <- dlsym lib "eclair_btree_destroy_test"
  funcPurge <- dlsym lib "eclair_btree_clear_test"
  funcSwap <- dlsym lib "eclair_btree_swap_test"
  funcBegin <- dlsym lib "eclair_btree_begin_test"
  funcEnd <- dlsym lib "eclair_btree_end_test"
  funcInsert <- dlsym lib "eclair_btree_insert_value_test"
  funcMerge <- dlsym lib "eclair_btree_insert_range_test"
  funcEmpty <- dlsym lib "eclair_btree_is_empty_test"
  funcSize <- dlsym lib "eclair_btree_size_test"
  funcNodeCount <- dlsym lib "eclair_btree_node_count_test"
  funcDepth <- dlsym lib "eclair_btree_depth_test"
  funcContains <- dlsym lib "eclair_btree_contains_helper_test"
  funcLB <- dlsym lib "eclair_btree_lower_bound_test"
  funcUB <- dlsym lib "eclair_btree_upper_bound_test"
  funcIterCurrent <- dlsym lib "eclair_btree_iterator_current_test"
  funcIterNext <- dlsym lib "eclair_btree_iterator_next_test"
  funcIterIsEqual <- dlsym lib "eclair_btree_iterator_is_equal_test"
  let withIter' :: forall a. (Ptr Iter -> IO a) -> IO a
      withIter' = mkWithX funcNewIter funcDeleteIter
      iterCurrent = mkIterCurrent funcIterCurrent
      begin' = mkBegin funcBegin
      end' = mkEnd funcEnd
  pure $ Bindings
    { dynamicLib = lib
    , withTree = mkWithX funcNewTree funcDeleteTree
    , withIter = withIter'
    , withValue = \value f -> do
        mkWithX funcNewValue funcDeleteValue $ \valuePtr -> do
          poke valuePtr value
          f valuePtr
    , bInit = mkInit funcInit
    , bDestroy = mkDestroy funcDestroy
    , bPurge = mkPurge funcPurge
    , bSwap = mkSwap funcSwap
    , bBegin = begin'
    , bEnd = end'
    , bInsert = mkInsert funcInsert
    , bMerge = mkMerge funcMerge withIter' begin' end'
    , bEmpty = mkIsEmpty funcEmpty
    , bSize = mkSize funcSize
    , bNodeCount = mkNodeCount funcNodeCount
    , bDepth = mkDepth funcDepth
    , bContains = mkContains funcContains
    , bIterCurrent = iterCurrent
    , bIterNext = mkIterNext funcIterNext
    , bIterIsEqual = mkIterIsEqual funcIterIsEqual
    , bLowerBound = mkBound funcLB withIter'
    , bUpperBound = mkBound funcUB withIter'
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
    mkMerge fn withIter' begin' end' tree1 tree2 = do
      withIter' $ \beginIter ->
        withIter' $ \endIter -> do
          R.void $ begin' tree2 beginIter
          R.void $ end' tree2 endIter
          callFFI fn retVoid [argPtr tree1, argPtr beginIter, argPtr endIter]
    mkIsEmpty fn tree = do
      result <- callFFI fn retCUChar [argPtr tree]
      pure $ result == 1
    mkSize fn tree = fromIntegral <$> callFFI fn retCULong [argPtr tree]
    mkNodeCount fn tree = fromIntegral <$> callFFI fn retCULong [argPtr tree]
    mkDepth fn tree = fromIntegral <$> callFFI fn retCUInt [argPtr tree]
    mkContains fn tree value = do
      result <- callFFI fn retCUChar [argPtr tree, argPtr value]
      pure $ result == 1
    mkNew fn = callFFI fn (retPtr retVoid) []
    mkDelete fn obj = callFFI fn retVoid [argPtr obj]
    mkWithX newFn deleteFn = bracket (castPtr <$> mkNew newFn) (mkDelete deleteFn)
    mkIterCurrent fn iter = castPtr <$> callFFI fn (retPtr retVoid) [argPtr iter]
    mkIterNext fn iter = callFFI fn retVoid [argPtr iter]
    mkIterIsEqual fn beginIter endIter = do
      result <- callFFI fn retCUChar [argPtr beginIter, argPtr endIter]
      pure $ result == 1
    mkBound fn withIter' tree value f = do
      withIter' $ \iter -> do
        callFFI fn retVoid [argPtr tree, argPtr value, argPtr iter]
        f iter

withIters :: Bindings -> (Ptr Iter -> Ptr Iter -> IO a) -> IO a
withIters bindings f =
  withIter bindings $ \beginIter ->
    withIter bindings $ \endIter ->
      f beginIter endIter

treeToList :: Bindings -> Ptr BTree -> IO [Value]
treeToList bindings tree =
  withIters bindings $ \beginIter endIter -> do
    bBegin bindings tree beginIter
    bEnd bindings tree endIter

    whileM (isNotEqualIter beginIter endIter) $ do
      value <- bIterCurrent bindings beginIter
      bIterNext bindings beginIter
      peek value
  where
    isNotEqualIter beginIter endIter = do
      not <$> bIterIsEqual bindings beginIter endIter

llFile, soFile :: FilePath -> FilePath
llFile dir = dir </> "btree.ll"
soFile dir = dir </> "btree.so"

testDir :: FilePath
testDir = "/tmp/eclair-btree"

notUsed :: a
notUsed = panic "Not used"

whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond action = go
  where
    go = cond >>= \case
      True -> do
        x <- action
        xs <- go
        pure $ x:xs
      False ->
        pure []

shuffle :: [a] -> IO [a]
shuffle xs = do
  array <- mkArray n xs
  forM [1..n] $ \i -> do
    j <- randomRIO (i,n)
    vi <- readArray array i
    vj <- readArray array j
    writeArray array j vi
    pure vj
  where
    n = length xs
    mkArray :: Int -> [a] -> IO (IOArray Int a)
    mkArray m = newListArray (1,m)

intoIO :: Identity a -> IO a
intoIO = pure . runIdentity
