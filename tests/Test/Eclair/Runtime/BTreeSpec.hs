{-# LANGUAGE MultiWayIf #-}

module Test.Eclair.Runtime.BTreeSpec
  ( module Test.Eclair.Runtime.BTreeSpec
  ) where

import Protolude hiding (Meta)
import Data.IORef
import Data.Maybe (fromJust)
import qualified Data.Text.Encoding as TE
import Test.Hspec
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
  :: FunPtr (Ptr Tree -> Ptr Iter -> IO ())
  -> Ptr Tree -> Ptr Iter -> IO ()
foreign import ccall "dynamic" foreignEnd
  :: FunPtr (Ptr Tree -> Ptr Iter -> IO ())
  -> Ptr Tree -> Ptr Iter -> IO ()
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

importSymbol :: CompileLayer l => l -> Text -> (FunPtr a -> a) -> IO a
importSymbol compileLayer symbol fn = do
  let hash = getHash settings
      hashedSymbol = symbol <> "_" <> unHash hash
      hashedSymbol' = toShort $ TE.encodeUtf8 hashedSymbol
  mangled <- mangleSymbol compileLayer hashedSymbol'
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

data Val = Val {-# UNPACK #-} !Int32 !Int32 !Int32 !Int32

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
