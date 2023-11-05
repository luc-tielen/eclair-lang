{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.SymbolUtils
  ( Bindings(..)
  , Symbol(..)
  , cgTestCode
  , loadNativeCode
  , loadNativeCode'
  , soFile
  , llFile
  ) where

import Prelude hiding (void, Symbol)
import Control.Exception
import Eclair.LLVM.Symbol
import Eclair.LLVM.Codegen hiding (retVoid, nullPtr)
import Eclair.LLVM.Externals
import Foreign.LibFFI
import Foreign hiding (void, bit)
import System.Posix.DynamicLinker
import System.FilePath
import Foreign.C

data Bindings
  = Bindings
  { dynamicLib :: DL
  , withSymbol :: forall a. (Ptr Symbol -> IO a) -> IO a
  , bInit :: Ptr Symbol -> String -> IO ()
  , bDestroy :: Ptr Symbol -> IO ()
  , bIsEqual :: Ptr Symbol -> Ptr Symbol -> IO Bool
  , bLength :: Ptr Symbol -> IO Word32
  , bData :: Ptr Symbol -> IO String
  }

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
  loadNativeCode' lib

loadNativeCode' :: DL -> IO Bindings
loadNativeCode' lib = do
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

soFile :: FilePath -> FilePath
soFile dir = dir </> "symbol.so"

llFile :: FilePath -> FilePath
llFile dir = dir </> "symbol.ll"
