{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Eclair.LLVM.SymbolTableSpec
  ( module Test.Eclair.LLVM.SymbolTableSpec
  ) where

import Prelude hiding (void, Symbol)
import qualified LLVM.C.API as LibLLVM
import qualified Test.Eclair.LLVM.SymbolUtils as S
import Control.Monad.Morph
import Control.Exception
import Eclair.LLVM.SymbolTable
import qualified Eclair.LLVM.Symbol as S
import qualified Eclair.LLVM.Vector as V
import qualified Eclair.LLVM.HashMap as HM
import Eclair.LLVM.Codegen hiding (retVoid, nullPtr)
import Eclair.LLVM.Externals
import System.Posix.DynamicLinker
import System.Directory.Extra
import System.Process.Extra
import System.FilePath
import Foreign hiding (void)
import Foreign.LibFFI
import Test.Hspec


type Symbol = S.Symbol
type Value = Word32

data Bindings
  = Bindings
  { dynamicLib :: DL
  , symBindings :: S.Bindings
  , withSymTab :: (Ptr SymbolTable -> IO ()) -> IO ()
  , bInit :: Ptr SymbolTable -> IO ()
  , bDestroy :: Ptr SymbolTable -> IO ()
  , bFindOrInsert :: Ptr SymbolTable -> Ptr Symbol -> IO Value
  -- NOTE: no need to free returned symbol after lookup
  , bLookupSymbol :: Ptr SymbolTable -> Value -> IO (Ptr Symbol)
  , bContainsSymbol :: Ptr SymbolTable -> Ptr Symbol -> IO Bool
  , bLookupIndex :: Ptr SymbolTable -> Ptr Symbol -> IO Value
  , bContainsIndex :: Ptr SymbolTable -> Value -> IO Bool
  }

spec :: Spec
spec = describe "Symbol table" $ aroundAll (setupAndTeardown testDir) $ parallel $ do
  it "can be initialized and destroyed" $ \bindings ->
    withSymTab bindings $ \st -> do
      bInit bindings st
      bDestroy bindings st

  it "is possible to add symbols to the table" $ \bindings -> do
    let sBindings = symBindings bindings
    withSymTab bindings $ \st -> do
      bInit bindings st
      _ <- S.withSymbol sBindings $ \sym -> do
        S.bInit sBindings sym "abcd"
        idx <- bFindOrInsert bindings st sym
        idx `shouldBe` 0
        idx' <- bFindOrInsert bindings st sym
        idx' `shouldBe` 0
        -- Owned by symbol table now:
        -- S.bDestroy sBindings sym

      _ <- S.withSymbol sBindings $ \sym -> do
        S.bInit sBindings sym "123"
        idx <- bFindOrInsert bindings st sym
        idx `shouldBe` 1
        -- Owned by symbol table now:
        -- S.bDestroy sBindings sym

      bDestroy bindings st

  it "is possible to check if the table contains a key" $ \bindings -> do
    let sBindings = symBindings bindings
    withSymTab bindings $ \st -> do
      bInit bindings st
      _ <- S.withSymbol sBindings $ \sym -> do
        S.bInit sBindings sym "abcd"
        result1 <- bContainsSymbol bindings st sym
        _ <- bFindOrInsert bindings st sym
        result2 <- bContainsSymbol bindings st sym
        S.bDestroy sBindings sym
        result1 `shouldBe` False
        result2 `shouldBe` True
      pass

  it "is possible to check if the table contains a value" $ \bindings -> do
    let sBindings = symBindings bindings
    withSymTab bindings $ \st -> do
      bInit bindings st
      _ <- S.withSymbol sBindings $ \sym -> do
        S.bInit sBindings sym "abcd"
        result1 <- bContainsIndex bindings st 0
        _ <- bFindOrInsert bindings st sym
        result2 <- bContainsIndex bindings st 0
        S.bDestroy sBindings sym
        result1 `shouldBe` False
        result2 `shouldBe` True
      pass

  it "is possible to lookup a key corresponding to a value" $ \bindings -> do
    let sBindings = symBindings bindings
    withSymTab bindings $ \st -> do
      bInit bindings st
      _ <- S.withSymbol sBindings $ \sym -> do
        S.bInit sBindings sym "abcd"
        -- NOTE: unsafe lookup, don't use it before symbol is inserted
        -- sym1 <- bLookupSymbol bindings st 0
        idx <- bFindOrInsert bindings st sym
        idx `shouldBe` 0
        print =<< bContainsSymbol bindings st sym
        sym' <- bLookupSymbol bindings st idx
        -- Owned by symbol table now:
        -- S.bDestroy sBindings sym
        result <- S.bIsEqual sBindings sym sym'
        result `shouldBe` True
        pass
      pass

  it "is possible to lookup a value corresponding to a key" $ \bindings -> do
    let sBindings = symBindings bindings
    withSymTab bindings $ \st -> do
      bInit bindings st
      _ <- S.withSymbol sBindings $ \sym -> do
        S.bInit sBindings sym "abcd"
        -- NOTE: unsafe lookup, don't use it before symbol is inserted
        idx1 <- bLookupIndex bindings st sym
        _ <- bFindOrInsert bindings st sym
        idx2 <- bLookupIndex bindings st sym
        S.bDestroy sBindings sym
        idx1 `shouldBe` 0xffffffff
        idx2 `shouldBe` 0
      pass

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
  -> (Symbol -> SymbolTable -> Externals -> ModuleBuilderT IO ())
  -> FilePath -> IO ()
compileCode cgExts cgHelperCode dir = do
  ctx <- LibLLVM.mkContext
  llvmMod <- LibLLVM.mkModule ctx "eclair"
  td <- LibLLVM.getTargetData llvmMod
  llvmIR <- runModuleBuilderT $ do
    exts <- cgExts
    symbol <- hoist intoIO $ S.codegen exts
    let cfg = Config Nothing ctx td
        symbolDestructor iterPtr = do
          _ <- call (S.symbolDestroy symbol) [iterPtr]
          pass
    vec <- instantiate "test" (S.tySymbol symbol) $ runConfigT cfg $ V.codegen exts (Just symbolDestructor)
    hm <- runConfigT cfg $ HM.codegen symbol exts
    symTab <- hoist intoIO $ codegen (S.tySymbol symbol) vec hm
    cgHelperCode symbol symTab exts
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

cgTestCode :: S.Symbol -> SymbolTable -> Externals -> ModuleBuilderT IO ()
cgTestCode sym symTab exts = do
  let mallocFn = extMalloc exts
      freeFn = extFree exts
      tySym = S.tySymbol sym
      symTabTy = tySymbolTable symTab
  _ <- function "eclair_symbol_table_new" [] (ptr symTabTy) $ \[] ->
    ret =<< call mallocFn [int32 4096]
  _ <- function "eclair_symbol_table_delete" [(ptr symTabTy, "hm")] void $ \[h] ->
    call freeFn [h]
  let args = [(ptr symTabTy, "symbol_table"), (ptr tySym, "symbol")]
  _ <- function "eclair_symbol_table_contains_symbol_helper" args i8 $ \[st, s] -> do
    result <- call (symbolTableContainsSymbol symTab) [st, s]
    ret =<< result `zext` i8
  let args' = [(ptr symTabTy, "symbol_table"), (i32, "value")]
  _ <- function "eclair_symbol_table_contains_index_helper" args' i8 $ \[st, v] -> do
    result <- call (symbolTableContainsIndex symTab) [st, v]
    ret =<< result `zext` i8

  S.cgTestCode sym exts

loadNativeCode :: FilePath -> IO Bindings
loadNativeCode dir = do
  lib <- dlopen (soFile dir) [RTLD_LAZY]
  sBindings <- S.loadNativeCode' lib
  fnNew <- dlsym lib "eclair_symbol_table_new"
  fnDelete <- dlsym lib "eclair_symbol_table_delete"
  fnInit <- dlsym lib "eclair_symbol_table_init"
  fnDestroy <- dlsym lib "eclair_symbol_table_destroy"
  fnFindOrInsert <- dlsym lib "eclair_symbol_table_find_or_insert"
  fnLookupSymbol <- dlsym lib "eclair_symbol_table_lookup_symbol"
  fnContainsSymbol <- dlsym lib "eclair_symbol_table_contains_symbol_helper"
  fnLookupIndex <- dlsym lib "eclair_symbol_table_lookup_index"
  fnContainsIndex <- dlsym lib "eclair_symbol_table_contains_index_helper"
  pure $ Bindings
    { dynamicLib = lib
    , symBindings = sBindings
    , withSymTab = mkWithSymTab fnNew fnDelete
    , bInit = mkInit fnInit
    , bDestroy = mkDestroy fnDestroy
    , bFindOrInsert = mkFindOrInsert fnFindOrInsert
    , bLookupSymbol = mkLookupSymbol fnLookupSymbol
    , bLookupIndex = mkLookupIndex fnLookupIndex
    , bContainsSymbol = mkContainsSymbol fnContainsSymbol
    , bContainsIndex = mkContainsIndex fnContainsIndex
    }
  where
    mkNew fn = callFFI fn (retPtr retVoid) []
    mkDelete fn st = callFFI fn retVoid [argPtr st]
    mkWithSymTab fnNew fnDelete =
      bracket (castPtr <$> mkNew fnNew) (mkDelete fnDelete)
    mkInit fn st = callFFI fn retVoid [argPtr st]
    mkDestroy fn st = callFFI fn retVoid [argPtr st]
    mkFindOrInsert fn st sym =
      fromIntegral <$> callFFI fn retCUInt [argPtr st, argPtr sym]
    mkLookupSymbol fn st value =
      castPtr <$> callFFI fn (retPtr retVoid) [argPtr st, argCUInt $ fromIntegral value]
    mkLookupIndex fn st sym =
      fromIntegral <$> callFFI fn retCUInt [argPtr st, argPtr sym]
    mkContainsSymbol fn st sym = do
      result <- callFFI fn retCUChar [argPtr st, argPtr sym]
      pure $ result == 1
    mkContainsIndex fn st value = do
      result <- callFFI fn retCUChar [argPtr st, argCUInt $ fromIntegral value]
      pure $ result == 1

testDir :: FilePath
testDir = "/tmp/eclair-symbol-table"

notUsed :: a
notUsed = panic "Not used"

intoIO :: Identity a -> IO a
intoIO = pure . runIdentity

llFile, soFile :: FilePath -> FilePath
llFile dir = dir </> "symbol-table.ll"
soFile dir = dir </> "symbol-table.so"
