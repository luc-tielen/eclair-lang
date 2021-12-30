module Test.Eclair.Runtime.BTreeSpec
  ( module Test.Eclair.Runtime.BTreeSpec
  ) where

import Protolude
import Test.Hspec
import Control.Monad.Cont
import Data.ByteString.Short hiding (index)
import Eclair.Runtime.BTree
import Eclair.Runtime.Store
import Eclair.Runtime.LLVM
import LLVM.IRBuilder.Module
import LLVM.Context
import LLVM.Module
import LLVM.Target
import LLVM.OrcJIT
import qualified LLVM.Relocation as Relocation
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.Internal.OrcJIT.CompileLayer as CL
import Foreign

foreign import ccall "dynamic" foreignInitEmpty
  :: FunPtr (Ptr Tree -> IO ()) -> Ptr Tree -> IO ()
foreign import ccall "dynamic" foreignDestroy
  :: FunPtr (Ptr Tree -> IO ()) -> Ptr Tree -> IO ()
foreign import ccall "dynamic" foreignPurge
  :: FunPtr (Ptr Tree -> IO ()) -> Ptr Tree -> IO ()
foreign import ccall "dynamic" foreignSwap
  :: FunPtr (Ptr Tree -> Ptr Tree -> IO ())
  -> Ptr Tree -> Ptr Tree -> IO ()
foreign import ccall "dynamic" foreignBegin
  :: FunPtr (Ptr Tree -> Ptr Value -> IO ())
  -> Ptr Tree -> Ptr Value -> IO ()
foreign import ccall "dynamic" foreignEnd
  :: FunPtr (Ptr Tree -> Ptr Value -> IO ())
  -> Ptr Tree -> Ptr Value -> IO ()
foreign import ccall "dynamic" foreignInsert
  :: FunPtr (Ptr Tree -> Ptr Value -> IO Bool)
  -> Ptr Tree -> Ptr Value -> IO Bool
foreign import ccall "dynamic" foreignInsertRange
  :: FunPtr (Ptr Tree -> Ptr Iter -> Ptr Iter -> IO ())
  -> Ptr Tree -> Ptr Iter -> Ptr Iter -> IO ()
foreign import ccall "dynamic" foreignIsEmpty
  :: FunPtr (Ptr Tree -> IO Bool)
  -> Ptr Tree -> IO Bool
foreign import ccall "dynamic" foreignLowerBound
  :: FunPtr (Ptr Tree -> Ptr Value -> Ptr Iter -> IO ())
  -> Ptr Tree -> Ptr Value -> Ptr Iter -> IO ()
foreign import ccall "dynamic" foreignUpperBound
  :: FunPtr (Ptr Tree -> Ptr Value -> Ptr Iter -> IO ())
  -> Ptr Tree -> Ptr Value -> Ptr Iter -> IO ()
foreign import ccall "dynamic" foreignContains
  :: FunPtr (Ptr Tree -> Ptr Value -> IO Bool)
  -> Ptr Tree -> Ptr Value -> IO Bool
foreign import ccall "dynamic" foreignIterIsEqual
  :: FunPtr (Ptr Iter -> Ptr Iter -> IO Bool)
  -> Ptr Iter -> Ptr Iter -> IO Bool
foreign import ccall "dynamic" foreignIterCurrent
  :: FunPtr (Ptr Iter -> IO (Ptr Value))
  -> Ptr Iter -> IO (Ptr Value)
foreign import ccall "dynamic" foreignIterNext
  :: FunPtr (Ptr Iter -> IO ())
  -> Ptr Iter -> IO ()


jit :: ModuleBuilderT IO a -> (forall l. CompileLayer l => l -> a -> IO b) -> IO b
jit code f = flip runContT pure $ do
  ctx <- ContT withContext
  (a, ffiCode) <- lift $ codegenModule "test.ll" code
  mod <- ContT $ withModuleFromAST ctx ffiCode
  tm <- ContT $ withHostTargetMachine Relocation.PIC CodeModel.JITDefault CodeGenOpt.None
  exeSession <- ContT withExecutionSession
  resolver <- ContT $ withSymbolResolver exeSession (SymbolResolver symbolResolver)
  objectLayer <- ContT $ withObjectLinkingLayer exeSession (\_key -> pure resolver)
  compileLayer <- ContT $ withIRCompileLayer objectLayer tm
  key <- ContT $ withModuleKey exeSession
  lift $ withModule compileLayer key mod $ f compileLayer a

symbolResolver :: MangledSymbol -> IO (Either JITSymbolError JITSymbol)
symbolResolver = panic "Not implemented!"

data Tree
data Value
data Iter

data FFI
  = FFI
  { ffiWithEmptyTree :: forall a. (Ptr Tree -> IO a) -> IO a
  , ffiPurge :: Ptr Tree -> IO ()
  , ffiSwap :: Ptr Tree -> Ptr Tree -> IO ()
  , ffiBegin :: Ptr Tree -> IO (ForeignPtr Value)
  , ffiEnd :: Ptr Tree -> IO (ForeignPtr Value)
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

jitCompile :: IO FFI
jitCompile = jit (codegen settings) $ \compileLayer (_, sizes) -> do
  fnInitEmpty <- importSymbol compileLayer "btree_init_empty" foreignInitEmpty
  fnDestroy <- importSymbol compileLayer "btree_destroy" foreignDestroy
  fnPurge <- importSymbol compileLayer "btree_clear" foreignPurge
  fnSwap <- importSymbol compileLayer "btree_swap" foreignSwap
  fnBegin <- importSymbol compileLayer "btree_begin" foreignBegin
  fnEnd <- importSymbol compileLayer "btree_end" foreignEnd
  fnInsert <- importSymbol compileLayer "btree_insert_value" foreignInsert
  fnInsertRange <- importSymbol compileLayer "btree_insert_range" foreignInsertRange
  fnIsEmpty <- importSymbol compileLayer "btree_is_empty" foreignIsEmpty
  fnLowerBound <- importSymbol compileLayer "btree_lower_bound" foreignLowerBound
  fnUpperBound <- importSymbol compileLayer "btree_upper_bound" foreignUpperBound
  fnContains <- importSymbol compileLayer "btree_contains" foreignContains
  fnIterIsEqual <- importSymbol compileLayer "iterator_is_equal" foreignIterIsEqual
  fnIterCurrent <- importSymbol compileLayer "iterator_current" foreignIterCurrent
  fnIterNext <- importSymbol compileLayer "iterator_next" foreignIterNext
  pure FFI { ffiWithEmptyTree = withResource (treeSize sizes) fnInitEmpty fnDestroy
           , ffiPurge = fnPurge
           , ffiSwap = fnSwap
           , ffiBegin = \tree -> allocateAndApply (valueSize sizes) (fnBegin tree)
           , ffiEnd = \tree -> allocateAndApply (valueSize sizes) (fnEnd tree)
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

importSymbol :: CompileLayer l => l -> ShortByteString -> (FunPtr a -> a) -> IO a
importSymbol compileLayer symbol fn = do
  mangled <- mangleSymbol compileLayer symbol
  Right (JITSymbol symbolPtr _) <- CL.findSymbol compileLayer mangled True
  pure $ fn $ castPtrToFunPtr $ wordPtrToPtr symbolPtr

withResource :: Word64            -- number of bytes to allocate
             -> (Ptr a -> IO ())  -- "constructor"
             -> (Ptr a -> IO ())  -- "destructor"
             -> (Ptr a -> IO b)
             -> IO b
withResource numBytes construct destruct =
  bracket (mask_ $ do
            ptr <- mallocBytes (fromIntegral numBytes)
            construct ptr
            pure ptr)
          (\ptr -> destruct ptr *> free ptr)

allocateAndApply :: Word64 -> (Ptr a -> IO b) -> IO (ForeignPtr a)
allocateAndApply size f = do
  ptr <- mallocForeignPtrBytes (fromIntegral size)
  withForeignPtr ptr f
  pure ptr

-- TODO: take specialized hash into account for function names
-- TODO: generate list of ops using hedgehog, run program with that

spec :: Spec
spec = describe "btree" $ beforeAll jitCompile $ parallel $ do
  it "can create and destroy btrees" $ \ffi -> pending

  it "can iterate over the full range of values" $ \ffi -> pending

  it "can use lower- and upper-bound to iterate over a subset of values" $ \ffi -> pending

  -- TODO: properties
  it "should be empty after purging" $ \ffi -> do
    pending

  describe "swap" $ parallel $ do
    it "swaps contents of tree A and B" $ \ffi -> pending

    it "is a no-op to swap twice" $ \ffi -> pending

  describe "insert" $ parallel $ do
    it "is not empty afterwards" $ \ffi -> pending

    it "does nothing if value is already stored in tree" $ \ffi -> pending

    it "adds the new value if not stored in tree" $ \ffi -> pending

    it "is commutative" $ \ffi -> pending

  describe "insertRange" $ parallel $ do
    it "increases in size by up to N when adding N elements" $ \ffi -> pending
    -- TODO same props as insert?

  describe "isEmpty" $ parallel $ do
    it "returns true for empty trees" $ \ffi -> pending

    it "returns false for non-empty trees" $ \ffi -> pending

  describe "contains" $ parallel $ do
    it "returns true if element is inside the tree" $ \ffi -> pending

    it "returns false if element is not inside the tree" $ \ffi -> pending
