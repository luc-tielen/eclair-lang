{-# LANGUAGE MultiWayIf, TypeApplications #-}

module Test.Eclair.Runtime.BTreeSpec
  ( module Test.Eclair.Runtime.BTreeSpec
  ) where

import Protolude hiding (Meta)
import Test.Hspec hiding (Arg)
import Test.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Text.Encoding as TE
import Control.Monad.Cont
import Data.ByteString.Short hiding (index)
import Eclair.Runtime.BTree
import Eclair.Runtime.Store
import Eclair.Runtime.LLVM hiding (nullPtr)
import Eclair.Runtime.Hash
import LLVM.IRBuilder.Module
import LLVM.Context
import LLVM.Module
import LLVM.Target
import LLVM.OrcJIT
import LLVM.Analysis
import qualified LLVM.Relocation as Relocation
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.Internal.OrcJIT.CompileLayer as CL
import Foreign
import Foreign.LibFFI


jit :: ModuleBuilderT IO a -> (forall l. CompileLayer l => l -> a -> IO b) -> IO b
jit code f = do
  cl <- newIORef Nothing
  flip runContT pure $ do
    ctx <- ContT withContext
    (a, ffiCode) <- lift $ codegenModule "test.ll" code
    mod <- ContT $ withModuleFromAST ctx ffiCode
    lift $ verify mod
    tm <- ContT $ withHostTargetMachine Relocation.PIC CodeModel.JITDefault CodeGenOpt.None
    exeSession <- ContT withExecutionSession
    resolver <- ContT $ withSymbolResolver exeSession (SymbolResolver $ symbolResolver cl)
    objectLayer <- ContT $ withObjectLinkingLayer exeSession (\_key -> pure resolver)
    compileLayer <- ContT $ withIRCompileLayer objectLayer tm
    lift $ writeIORef cl (Just compileLayer)
    key <- ContT $ withModuleKey exeSession
    lift $ withModule compileLayer key mod $ f compileLayer a

symbolResolver :: CompileLayer l => IORef (Maybe l) -> MangledSymbol -> IO (Either JITSymbolError JITSymbol)
symbolResolver compileLayer symbol = do
  cl <- fromJust <$> readIORef compileLayer
  malloc <- mangleSymbol cl "malloc"
  freeSym <- mangleSymbol cl "free"
  if | symbol == malloc -> do
      funPtr <- wrapMalloc $ \int -> mallocBytes (fromIntegral int)
      let addr = ptrToWordPtr (castFunPtrToPtr funPtr)
      pure $ Right $ JITSymbol addr defaultJITSymbolFlags
     | symbol == freeSym -> do
      funPtr <- wrapFree free
      let addr = ptrToWordPtr (castFunPtrToPtr funPtr)
      pure $ Right $ JITSymbol addr defaultJITSymbolFlags
     | otherwise -> panic $ "Can't resolve symbol: " <> show symbol

foreign import ccall "wrapper" wrapMalloc
  :: (Int64 -> IO (Ptr a)) -> IO (FunPtr (Int64 -> IO (Ptr a)))
foreign import ccall "wrapper" wrapFree
  :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

data Tree
data Value
data Iter

data FFI
  = FFI
  { ffiWithEmptyTree :: forall a. (Ptr Tree -> IO a) -> IO a
  , ffiPurge :: Ptr Tree -> IO ()
  , ffiSwap :: Ptr Tree -> Ptr Tree -> IO ()
  , ffiBegin :: Ptr Tree -> IO (ForeignPtr Iter)
  , ffiEnd :: Ptr Tree -> IO (ForeignPtr Iter)
  , ffiInsert :: Ptr Tree -> Ptr Value -> IO Bool
  , ffiInsertRange :: Ptr Tree -> Ptr Iter -> Ptr Iter -> IO ()
  , ffiIsEmpty :: Ptr Tree -> IO Bool
  , ffiLowerBound :: Ptr Tree -> Ptr Value -> IO (ForeignPtr Iter)
  , ffiUpperBound :: Ptr Tree -> Ptr Value -> IO (ForeignPtr Iter)
  , ffiContains :: Ptr Tree -> Ptr Value -> IO Bool
  , ffiIterIsEqual :: Ptr Iter -> Ptr Iter -> IO Bool
  , ffiIterCurrent :: Ptr Iter -> IO (Ptr Value)
  , ffiIterNext :: Ptr Iter -> IO ()
  }

settings :: Meta
settings
  = Meta
  { numColumns = 4
  , index = [1, 3]
  , blockSize = 256
  , searchType = Linear
  }

jitCompile :: (FFI -> IO ()) -> IO ()
jitCompile action = jit (codegen settings) $ \compileLayer (_, sizes) -> do
  fnInitEmpty <- importSymbol compileLayer "btree_init_empty"
  fnDestroy <- importSymbol compileLayer "btree_destroy"
  fnPurge <- importSymbol compileLayer "btree_clear"
  fnSwap <- importSymbol compileLayer "btree_swap"
  fnBegin <- importSymbol @(Ptr Tree -> Ptr Iter -> IO ()) compileLayer "btree_begin"
  fnEnd <- importSymbol @(Ptr Tree -> Ptr Iter -> IO ()) compileLayer "btree_end"
  fnInsert <- importSymbol compileLayer "btree_insert_value"
  fnInsertRange <- importSymbol compileLayer "btree_insert_range"
  fnIsEmpty <- importSymbol compileLayer "btree_is_empty"
  fnLowerBound <- importSymbol @(Ptr Tree -> Ptr Value -> Ptr Iter -> IO ()) compileLayer "btree_lower_bound"
  fnUpperBound <- importSymbol @(Ptr Tree -> Ptr Value -> Ptr Iter -> IO ()) compileLayer "btree_upper_bound"
  fnContains <- importSymbol compileLayer "btree_contains"
  fnIterIsEqual <- importSymbol compileLayer "iterator_is_equal"
  fnIterCurrent <- importSymbol compileLayer "iterator_current"
  fnIterNext <- importSymbol compileLayer "iterator_next"
  let ffi = FFI { ffiWithEmptyTree = withResource (treeSize sizes) fnInitEmpty fnDestroy
                , ffiPurge = fnPurge
                , ffiSwap = fnSwap
                , ffiBegin = allocateAndApply (valueSize sizes) . fnBegin
                , ffiEnd = allocateAndApply (valueSize sizes) . fnEnd
                , ffiInsert = fnInsert
                , ffiInsertRange = fnInsertRange
                , ffiIsEmpty = fnIsEmpty
                , ffiLowerBound = \tree val -> allocateAndApply (iterSize sizes) (fnLowerBound tree val)
                , ffiUpperBound = \tree val -> allocateAndApply (iterSize sizes) (fnUpperBound tree val)
                , ffiContains = fnContains
                , ffiIterIsEqual = fnIterIsEqual
                , ffiIterCurrent = fnIterCurrent
                , ffiIterNext = fnIterNext
                }
  action ffi

importSymbol :: Import a => CompileLayer l => l -> Text -> IO a
importSymbol compileLayer symbol = do
  let hash = getHash settings
      hashedSymbol = symbol <> "_" <> unHash hash
      hashedSymbol' = toShort $ TE.encodeUtf8 hashedSymbol
  mangled <- mangleSymbol compileLayer hashedSymbol'
  Right (JITSymbol symbolPtr _) <- CL.findSymbol compileLayer mangled True
  wrap $ castPtrToFunPtr $ wordPtrToPtr symbolPtr

class Import a where
  wrap :: FunPtr a -> IO a

instance Import (Ptr a -> IO ()) where
  wrap fn = do
    pure $ \ptr -> do
      callFFI fn retVoid [argPtr ptr]

instance Import (Ptr a -> IO Bool) where
  wrap fn = do
    pure $ \ptr -> (== 1) <$> callFFI fn retCInt [argPtr ptr]

instance Import (Ptr a -> IO (Ptr Value)) where
  wrap fn = do
    pure $ \ptr -> callFFI fn (retPtr (panic "Unused arg")) [argPtr ptr]

instance Import (Ptr a -> Ptr b -> IO ()) where
  wrap fn = do
    pure $ \ptr1 ptr2 -> callFFI fn retVoid [argPtr ptr1, argPtr ptr2]

instance Import (Ptr a -> Ptr b -> IO Bool) where
  wrap fn = do
    pure $ \ptr1 ptr2 -> (== 1) <$> callFFI fn retCInt [argPtr ptr1, argPtr ptr2]

instance Import (Ptr a -> Ptr b -> Ptr c -> IO ()) where
  wrap fn = do
    pure $ \ptr1 ptr2 ptr3 -> callFFI fn retVoid [argPtr ptr1, argPtr ptr2, argPtr ptr3]

instance Import (Ptr a -> Ptr b -> Ptr c -> IO Bool) where
  wrap fn = do
    pure $ \ptr1 ptr2 ptr3 -> (== 1) <$> callFFI fn retCInt [argPtr ptr1, argPtr ptr2, argPtr ptr3]


withResource :: Word64            -- number of bytes to allocate
             -> (Ptr a -> IO ())  -- "constructor"
             -> (Ptr a -> IO ())  -- "destructor"
             -> (Ptr a -> IO b)
             -> IO b
withResource numBytes construct destruct f = do
  allocaBytes (fromIntegral numBytes) $ \ptr -> do
    bracket (construct ptr $> ptr) destruct f

allocateAndApply :: Word64 -> (Ptr a -> IO b) -> IO (ForeignPtr a)
allocateAndApply size f = do
  ptr <- mallocForeignPtrBytes (fromIntegral size)
  withForeignPtr ptr f
  pure ptr

data Val = Val {-# UNPACK #-} !Int32 !Int32 !Int32 !Int32
  deriving (Eq, Show)

putValue :: Val -> IO (ForeignPtr Value)
putValue (Val x0 x1 x2 x3) = do
  ptr <- mallocForeignPtrBytes 16
  withForeignPtr ptr $ \ptr' -> do
    let p = castPtr ptr'
    pokeElemOff p 0 x0
    pokeElemOff p 1 x1
    pokeElemOff p 2 x2
    pokeElemOff p 3 x3
  pure ptr

getValue :: Ptr Value -> IO Val
getValue ptr = do
  let p = castPtr ptr
  x0 <- peekElemOff p 0
  x1 <- peekElemOff p 1
  x2 <- peekElemOff p 2
  x3 <- peekElemOff p 3
  pure $ Val x0 x1 x2 x3

getAllValues :: FFI -> Ptr Tree -> IO [Val]
getAllValues ffi tree = do
  begin <- ffiBegin ffi tree
  end <- ffiEnd ffi tree
  withForeignPtr begin $ \begin' ->
    withForeignPtr end $ \end' ->
      go begin' end' []
  where
    go current end result = do
      isEqual <- ffiIterIsEqual ffi current end
      if isEqual
        then pure result
        else do
          val <- getValue =<< ffiIterCurrent ffi current
          ffiIterNext ffi current
          go current end (val : result)

runContT_ :: Monad m => ContT () m a -> m ()
runContT_ = flip runContT (const $ pure ())

genValue :: MonadGen m => m Val
genValue = Val <$> genInt <*> genInt <*> genInt <*> genInt
  where genInt = Gen.int32 (Range.linear 0 10000)

genValues :: MonadGen m => m [Val]
genValues = Gen.list (Range.linear 1 100) genValue

main :: IO ()
main = jitCompile $ \ffi -> hspec $ do
  describe "btree" $ parallel $ do
    it "can create and destroy btrees" $ do
        ffiWithEmptyTree ffi $ \tree ->
          tree `shouldNotBe` nullPtr

    -- TODO: try with empty tree
    -- TODO: try with tree that contains values
    it "can iterate over the full range of values" $ pending

    it "can use lower- and upper-bound to iterate over a subset of values" $ pending

    describe "is_empty" $ parallel $ do
      it "should return True for empty trees" $ hedgehog $ do
        isEmpty <- lift $ ffiWithEmptyTree ffi $ ffiIsEmpty ffi
        isEmpty === True

      it "should return False for non-empty trees" $ hedgehog $ do
        vals <- forAll genValues

        isEmpty <- lift $ ffiWithEmptyTree ffi $ \tree -> do
          for_ vals $ \val -> do
            val' <- putValue val
            withForeignPtr val' $ ffiInsert ffi tree
          ffiIsEmpty ffi tree

        isEmpty === False

      it "should be empty after purging" $ do
        pending

    describe "swap" $ parallel $ do
      it "swaps contents of tree A and B" $ pending

      it "is a no-op to swap twice" $ pending

    describe "insert" $ parallel $ do
      it "is not empty afterwards" $ pending

      it "does nothing if value is already stored in tree" $ pending

      it "adds the new value if not stored in tree" $ pending

      it "is commutative" $ pending

    describe "insertRange" $ parallel $ do
      it "increases in size by up to N when adding N elements" $ pending
      -- TODO same props as insert?

    describe "isEmpty" $ parallel $ do
      it "returns true for empty trees" $ pending

      it "returns false for non-empty trees" $ pending

    describe "contains" $ parallel $ do
      it "returns true if element is inside the tree" $ pending

      it "returns false if element is not inside the tree" $ pending
