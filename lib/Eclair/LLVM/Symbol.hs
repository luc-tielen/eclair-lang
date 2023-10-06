{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Eclair.LLVM.Symbol
  ( Symbol(..)
  , codegen
  , sizeOf
  , dataOf
  ) where

import Prelude hiding (Symbol, void)
import Eclair.LLVM.Codegen
import Eclair.LLVM.Externals


data Symbol
  = Symbol
  { tySymbol :: Type  -- A symbol (string). Assumes UTF-8 encoding.
  , symbolInit :: Operand
  , symbolDestroy :: Operand
  , symbolIsEqual :: Operand
  }

data CGState
  = CGState
  { externals :: Externals
  , tySym :: Type
  }

type ModuleCodegen = ReaderT CGState ModuleBuilder

codegen :: Externals -> ModuleBuilder Symbol
codegen exts = do
  symbolTy <- generateTypes
  runReaderT generateFunctions $ CGState exts symbolTy

generateTypes :: ModuleBuilder Type
generateTypes =
  -- For now, only up to 4GB of strings are supported.
  -- TODO consider strings with i8 and i16 as size also
  typedef "symbol_t" On [i32, ptr i8]

generateFunctions :: ModuleCodegen Symbol
generateFunctions = do
  symbolTy <- asks tySym
  symInit <- mkSymbolInit
  symDestroy <- mkSymbolDestroy
  symIsEqual <- mkSymbolIsEqual
  pure $ Symbol
    { tySymbol = symbolTy
    , symbolInit = symInit
    , symbolDestroy = symDestroy
    , symbolIsEqual = symIsEqual
    }

-- NOTE: this copies data to the new struct, and assumes memory has already been
-- allocated for the symbol. Don't pass a const char* / ptr i8 that point to
-- static memory because it will be freed during cleanup.
mkSymbolInit :: ModuleCodegen Operand
mkSymbolInit = do
  symbolTy <- asks tySym
  let args = [(ptr symbolTy, "symbol"), (i32, "size"), (ptr i8, "data")]
  function "eclair_symbol_init" args void $ \[symbol, size, utf8Data] -> do
    -- assert(symbol && "symbol cannot be NULL!");
    -- assert(utf8Data && "data cannot be NULL!");
    assign sizeOf symbol size
    assign dataOf symbol utf8Data

-- NOTE: this only destroys the memory this symbol is pointing to.
mkSymbolDestroy :: ModuleCodegen Operand
mkSymbolDestroy = do
  (symbolTy, freeFn) <- asks (tySym &&& extFree . externals)
  let args = [(ptr symbolTy, "symbol")]
  function "eclair_symbol_destroy" args void $ \[symbol] -> do
    -- assert(symbol && "symbol cannot be NULL!");
    dataPtr <- deref dataOf symbol
    call freeFn [dataPtr]

mkSymbolIsEqual :: ModuleCodegen Operand
mkSymbolIsEqual = do
  (symbolTy, memcmpFn) <- asks (tySym &&& extMemcmp . externals)
  let args = [(ptr symbolTy, "symbol1"), (ptr symbolTy, "symbol2")]
  function "eclair_symbol_is_equal" args i1 $ \[symbol1, symbol2] -> do
    -- assert(symbol1 && "symbol1 cannot be NULL!");
    -- assert(symbol2 && "symbol2 cannot be NULL!");
    size1 <- deref sizeOf symbol1
    size2 <- deref sizeOf symbol2
    isNotEqualSize <- size1 `ne` size2
    if' isNotEqualSize $
      ret $ bit 0

    data1 <- deref dataOf symbol1
    data2 <- deref dataOf symbol2
    size1' <- zext size1 i64
    isDataEqual <- (`eq` bit 0) =<< call memcmpFn [data1, data2, size1']
    ret isDataEqual

data Index
  = SymbolIdx
  | SizeIdx
  | DataIdx

sizeOf :: Path 'SymbolIdx 'SizeIdx
sizeOf = mkPath [int32 0]

dataOf :: Path 'SymbolIdx 'DataIdx
dataOf = mkPath [int32 1]
