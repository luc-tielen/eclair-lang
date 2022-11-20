{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Eclair.LLVM.SymbolTable
  ( SymbolTable(..)
  , codegen
  ) where

import Prelude hiding (void)
import LLVM.Codegen
import qualified Eclair.LLVM.Vector as Vector
import qualified Eclair.LLVM.HashMap as HashMap

data SymbolTable
  = SymbolTable
  { tySymbolTable :: Type
  , symbolTableInit :: Operand
  , symbolTableDestroy :: Operand
  , symbolTableFindOrInsert :: Operand
  , symbolTableContainsIndex :: Operand
  , symbolTableContainsSymbol :: Operand
  , symbolTableLookupIndex :: Operand
  , symbolTableLookupSymbol :: Operand
  }

data CGState
  = CGState
  { symbolTableTy :: Type
  , symbolTy :: Type
  , vectorCodegen :: Vector.Vector
  , hashMapCodegen :: HashMap.HashMap
  }

type ModuleCodegen = ReaderT CGState ModuleBuilder


codegen :: Type -> Vector.Vector -> HashMap.HashMap -> ModuleBuilder SymbolTable
codegen symbolTy' vec hm = do
  let vecTy = Vector.tyVector $ Vector.vectorTypes vec
      hmTy = HashMap.tyHashMap $ HashMap.hashMapTypes hm
  ty <- typedef "symbol_table" Off [ vecTy  -- maps indexes (i32) to symbols
                                   , hmTy   -- maps symbols to indexes
                                   ]
  runReaderT generateFunctions $ CGState ty symbolTy' vec hm

generateFunctions :: ModuleCodegen SymbolTable
generateFunctions = do
  ty <- asks symbolTableTy
  stInit <- mkSymbolTableInit
  stDestroy <- mkSymbolTableDestroy
  stFindOrInsert <- mkSymbolTableFindOrInsert
  stContainsIndex <- mkSymbolTableContainsIndex
  stContainsSymbol <- mkSymbolTableContainsSymbol
  stLookupIndex <- mkSymbolTableLookupIndex
  stLookupSymbol <- mkSymbolTableLookupSymbol
  pure $ SymbolTable
    { tySymbolTable = ty
    , symbolTableInit = stInit
    , symbolTableDestroy = stDestroy
    , symbolTableFindOrInsert = stFindOrInsert
    , symbolTableContainsIndex = stContainsIndex
    , symbolTableContainsSymbol = stContainsSymbol
    , symbolTableLookupIndex = stLookupIndex
    , symbolTableLookupSymbol = stLookupSymbol
    }

mkSymbolTableInit :: ModuleCodegen Operand
mkSymbolTableInit = do
  CGState ty _ vec hm <- ask

  function "symbol_table_init" [(ptr ty, "table")] void $ \[symTab] -> do
    -- assert(table && "symbol table cannot be NULL!");
    vecPtr <- addr vecOf symTab
    hmPtr <- addr hashMapOf symTab

    _ <- call (Vector.vectorInit vec) [vecPtr]
    _ <- call (HashMap.hashMapInit hm) [hmPtr]
    pass

mkSymbolTableDestroy :: ModuleCodegen Operand
mkSymbolTableDestroy = do
  CGState ty _ vec hm <- ask

  function "symbol_table_destroy" [(ptr ty, "table")] void $ \[symTab] -> do
    -- assert(table && "symbol table cannot be NULL!");
    vecPtr <- addr vecOf symTab
    hmPtr <- addr hashMapOf symTab

    _ <- call (Vector.vectorDestroy vec) [vecPtr]
    _ <- call (HashMap.hashMapDestroy hm) [hmPtr]
    pass

mkSymbolTableFindOrInsert :: ModuleCodegen Operand
mkSymbolTableFindOrInsert = do
  CGState ty symbolTy' vec hm <- ask
  let args = [(ptr ty, "table"), (ptr symbolTy', "symbol")]

  function "symbol_table_find_or_insert" args i32 $ \[symTabPtr, symbolPtr] -> do
    -- assert(table && "symbol table cannot be NULL!");
    vecPtr <- addr vecOf symTabPtr
    hmPtr <- addr hashMapOf symTabPtr

    count <- call (Vector.vectorSize vec) [vecPtr]
    value <- call (HashMap.hashMapGetOrPutValue hm) [hmPtr, symbolPtr, count]

    isInsertOfNewValue <- count `eq` value
    if' isInsertOfNewValue $ do
      -- New value is being inserted, so we need to update mapping in other direction.
      call (Vector.vectorPush vec) [vecPtr, symbolPtr]

    ret value

mkSymbolTableContainsIndex :: ModuleCodegen Operand
mkSymbolTableContainsIndex = do
  CGState ty _ vec _ <- ask
  let args = [(ptr ty, "table"), (i32, "index")]

  function "symbol_table_contains_index" args i1 $ \[symTabPtr, idx] -> do
    -- assert(table && "symbol table cannot be NULL!");
    vecPtr <- addr vecOf symTabPtr

    size <- call (Vector.vectorSize vec) [vecPtr]
    ret =<< idx `ult` size

mkSymbolTableContainsSymbol :: ModuleCodegen Operand
mkSymbolTableContainsSymbol = do
  CGState ty symbolTy' _ hm <- ask
  let args = [(ptr ty, "table"), (ptr symbolTy', "symbol")]

  function "symbol_table_contains_symbol" args i1 $ \[symTabPtr, symbolPtr] -> do
    hmPtr <- addr hashMapOf symTabPtr
    ret =<< call (HashMap.hashMapContains hm) [hmPtr, symbolPtr]

mkSymbolTableLookupIndex :: ModuleCodegen Operand
mkSymbolTableLookupIndex = do
  CGState ty symbolTy' _ hm <- ask
  let args = [(ptr ty, "table"), (ptr symbolTy', "symbol")]

  function "symbol_table_lookup_index" args i32 $ \[symTabPtr, symbolPtr] -> do
    hmPtr <- addr hashMapOf symTabPtr
    ret =<< call (HashMap.hashMapLookup hm) [hmPtr, symbolPtr]

mkSymbolTableLookupSymbol :: ModuleCodegen Operand
mkSymbolTableLookupSymbol = do
  CGState ty symbolTy' vec _ <- ask
  let args = [(ptr ty, "table"), (i32, "index")]

  function "symbol_table_lookup_symbol" args (ptr symbolTy') $ \[symTabPtr, idx] -> do
    -- assert(symbol_table_contains_index(table, index));
    vecPtr <- addr vecOf symTabPtr
    ret =<< call (Vector.vectorGetValue vec) [vecPtr, idx]

-- Helpers

data Index
  = SymbolTableIdx
  | VecIdx
  | HashMapIdx

vecOf :: Path 'SymbolTableIdx 'VecIdx
vecOf = mkPath [int32 0]

hashMapOf :: Path 'SymbolTableIdx 'HashMapIdx
hashMapOf = mkPath [int32 1]
