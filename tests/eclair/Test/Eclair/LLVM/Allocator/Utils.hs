module Test.Eclair.LLVM.Allocator.Utils
  ( Bindings(..)
  , compileAllocatorCode
  , loadNativeCode
  ) where

import System.Process.Extra
import System.FilePath
import System.Posix.DynamicLinker
import Eclair.LLVM.Codegen hiding (retVoid, nullPtr)
import Eclair.LLVM.Allocator.Common
import Control.Monad.Morph
import Control.Exception
import Foreign.LibFFI
import Foreign.Ptr
import Foreign.C

type I8 = CUChar

data Bindings a
  = Bindings
  { dynamicLib :: DL
  , withAlloc :: (Ptr a -> IO ()) -> IO ()
  , fnAlloc :: Ptr a -> CSize -> IO (Ptr I8)
  , fnFree :: Ptr a -> Ptr I8 -> CSize -> IO ()
  , fnInit :: Ptr a -> IO ()
  , fnDestroy :: Ptr a -> IO ()
  }

compileAllocatorCode
  :: Allocator a
  -> Text
  -> ModuleBuilderT IO Externals
  -> (Type -> Operand -> Operand -> ModuleBuilderT IO ())
  -> FilePath -> IO ()
compileAllocatorCode allocator prefix cgExts cgHelperCode dir = do
  llvmIR <- runModuleBuilderT $ do
    exts <- cgExts
    let cgBlueprint = flip evalStateT exts $ cgAlloc prefix allocator
    blueprint <- hoist intoIO cgBlueprint
    cgHelperCode (bpType blueprint) (extMalloc exts) (extFree exts)
  let llvmIRText = ppllvm llvmIR
  writeFileText (llFile dir) llvmIRText
  callProcess "clang" ["-fPIC", "-shared", "-O0", "-o", soFile dir, llFile dir]

intoIO :: Identity a -> IO a
intoIO = pure . runIdentity

llFile, soFile :: FilePath -> FilePath
llFile dir = dir </> "allocator.ll"
soFile dir = dir </> "allocator.so"

loadNativeCode :: Text -> FilePath -> IO (Bindings a)
loadNativeCode (toString -> pfx) dir = do
  lib <- dlopen (soFile dir) [RTLD_LAZY]
  newFn <- dlsym lib (pfx <> "_new")
  deleteFn <- dlsym lib (pfx <> "_delete")
  allocFn <- dlsym lib (pfx <> "_alloc")
  freeFn <- dlsym lib (pfx <> "_free")
  initFn <- dlsym lib (pfx <> "_init")
  destroyFn <- dlsym lib (pfx <> "_destroy")
  pure $ Bindings
    { dynamicLib = lib
    , withAlloc = mkWithAlloc newFn deleteFn
    , fnAlloc = mkAlloc allocFn
    , fnFree = mkFree freeFn
    , fnInit = mkInit initFn
    , fnDestroy = mkDestroy destroyFn
    }
  where
    mkAlloc fn mallocator numBytes =
      callFFI fn (retPtr retCUChar)
        [ argPtr mallocator
        , argCUInt $ fromIntegral numBytes
        ]
    mkFree fn mallocator memory numBytes =
      callFFI fn retVoid
        [ argPtr mallocator
        , argPtr memory
        , argCSize $ fromIntegral numBytes
        ]
    mkInit fn mallocator =
      callFFI fn retVoid [argPtr mallocator]
    mkDestroy fn mallocator =
      callFFI fn retVoid [argPtr mallocator]
    mkNew fn =
      callFFI fn (retPtr retVoid) []
    mkDelete fn mallocator =
      callFFI fn retVoid [argPtr mallocator]
    mkWithAlloc newFn deleteFn =
      bracket (castPtr <$> mkNew newFn) (mkDelete deleteFn)
