module Eclair.LLVM.HashMap
  ( HashMap(..)
  , Types(..)
  , codegen
  ) where

import Prelude hiding (void, HashMap)
import Control.Monad.Morph
import Eclair.LLVM.Codegen
import Eclair.LLVM.Externals
import qualified Eclair.LLVM.Symbol as Symbol
import qualified Eclair.LLVM.Vector as Vector

-- NOTE: this is a really naive hashmap (no growing / re-hashing yet),
-- should be replaced by something more sophisticated, like:
-- https://github.com/souffle-lang/souffle/blob/d1522e06c6e99c259951dc348689a77fa5f5c932/src/include/souffle/datastructure/ConcurrentInsertOnlyHashMap.h

data Types
  = Types
  { tyHashMap :: Type
  , tyKey :: Type
  , tyValue :: Type
  , tyEntry :: Type  -- struct containing key + value type
  }

data HashMap
  = HashMap
  { hashMapTypes :: Types
  , hashMapInit :: Operand
  , hashMapDestroy :: Operand
  , hashMapGetOrPutValue :: Operand
  , hashMapLookup :: Operand
  , hashMapContains :: Operand
  }

data CGState
  = CGState
  { _externals :: Externals
  , types :: Types
  , symbolCodegen :: Symbol.Symbol
  , vectorCodegen :: Vector.Vector
  }

type ModuleCodegen = ReaderT CGState ModuleBuilder
type IRCodegen = IRBuilderT ModuleCodegen


-- NOTE: no need to turn into template (for now)
codegen :: Symbol.Symbol -> Externals -> ConfigT (ModuleBuilderT IO) HashMap
codegen symbol exts = do
  let keyTy = Symbol.tySymbol symbol
      valueTy = i32
  entryTy <- typedef "entry_t" Off [keyTy, valueTy]
  vec <- hoist (instantiate "entry" entryTy) $ Vector.codegen exts Nothing
  lift $ do
    let vecTy = Vector.tyVector $ Vector.vectorTypes vec
    hashMapTy <- typedef "hashmap_t" Off [ArrayType capacity vecTy]
    let tys = Types
          { tyHashMap = hashMapTy
          , tyKey = keyTy
          , tyValue = valueTy
          , tyEntry = entryTy
          }

    hoist intoIO $ runReaderT generateFunctions $ CGState exts tys symbol vec
  where
    intoIO = pure . runIdentity

-- TODO make this variable sized
capacity :: Word32
capacity = 64

generateFunctions :: ModuleCodegen HashMap
generateFunctions = do
  tys <- asks types
  hmHash <- mkHash
  hmInit <- mkHashMapInit
  hmDestroy <- mkHashMapDestroy
  hmGetOrPutValue <- mkHashMapGetOrPutValue hmHash
  hmLookup <- mkHashMapLookup hmHash
  hmContains <- mkHashMapContains hmHash
  pure $ HashMap
    { hashMapTypes = tys
    , hashMapInit = hmInit
    , hashMapDestroy = hmDestroy
    , hashMapGetOrPutValue = hmGetOrPutValue
    , hashMapLookup = hmLookup
    , hashMapContains = hmContains
    }

mkHash :: ModuleCodegen Operand
mkHash = do
  symbolTy <- symbolType
  let hashTy = i32

  -- TODO better hash function?
  function "symbol_hash" [(ptr symbolTy, "symbol")] hashTy $ \[symbol] -> do
    hashPtr <- allocate hashTy (int32 0)
    symbolSize <- deref Symbol.sizeOf symbol
    dataPtr <- deref Symbol.dataOf symbol

    loopFor (int32 0) (`ult` symbolSize) (add (int32 1)) $ \i -> do
      -- We need a raw gep here, since the data is dynamically allocated.
      bytePtr <- gep dataPtr [i]
      byte <- load bytePtr 0 >>= (`zext` i32)
      currentHashCode <- load hashPtr 0
      result1 <- mul (int32 31) currentHashCode
      result2 <- add byte result1
      store hashPtr 0 result2

    hashCode <- load hashPtr 0
    ret =<< modulo hashCode capacity

mkHashMapInit :: ModuleCodegen Operand
mkHashMapInit = do
  (hmTy, vec) <- asks (tyHashMap . types &&& vectorCodegen)

  function "hashmap_init" [(ptr hmTy, "hashmap")] void $ \[hm] -> do
    loopBuckets hm $ \bucketPtr -> do
      call (Vector.vectorInit vec) [bucketPtr]

mkHashMapDestroy :: ModuleCodegen Operand
mkHashMapDestroy = do
  (hmTy, vec) <- asks (tyHashMap . types &&& vectorCodegen)

  function "hashmap_destroy" [(ptr hmTy, "hashmap")] void $ \[hm] -> do
    loopBuckets hm $ \bucketPtr -> do
      call (Vector.vectorDestroy vec) [bucketPtr]

mkHashMapGetOrPutValue :: Operand -> ModuleCodegen Operand
mkHashMapGetOrPutValue hashFn = do
  (hmTy, entryTy) <- asks ((tyHashMap &&& tyEntry) . types)
  symbolTy <- asks (Symbol.tySymbol . symbolCodegen)
  vec <- asks vectorCodegen

  let args = [(ptr hmTy, "hashmap"), (ptr symbolTy, "symbol"), (i32, "value")]

  function "hashmap_get_or_put_value" args i32 $ \[hm, symbolPtr, value] -> do
    bucketPtr <- bucketForHash hashFn hm symbolPtr
    loopEntriesInBucket symbolPtr bucketPtr $
      ret <=< deref valueOf

    symbolValue <- load symbolPtr 0
    newEntryPtr <- alloca entryTy Nothing 0
    assign symbolOf newEntryPtr symbolValue
    assign valueOf newEntryPtr value
    _ <- call (Vector.vectorPush vec) [bucketPtr, newEntryPtr]
    ret value

-- NOTE: this is a unsafe lookup, assumes element is in there!
-- NOTE: the index 0xFFFFFFFF is assumed to mean: not found.
-- This should be a safe assumption as long as there are less keys than this in the hashmap
mkHashMapLookup :: Operand -> ModuleCodegen Operand
mkHashMapLookup hashFn = do
  (hmTy, symbolTy) <- asks (tyHashMap . types &&& Symbol.tySymbol . symbolCodegen)
  let args = [(ptr hmTy, "hashmap"), (ptr symbolTy, "symbol")]
  function "hashmap_lookup" args i32 $ \[hm, symbolPtr] -> do
    bucketPtr <- bucketForHash hashFn hm symbolPtr
    loopEntriesInBucket symbolPtr bucketPtr $
      ret <=< deref valueOf

    ret $ int32 0xFFFFFFFF

mkHashMapContains :: Operand -> ModuleCodegen Operand
mkHashMapContains hashFn = do
  (hmTy, symbolTy) <- asks (tyHashMap . types &&& Symbol.tySymbol . symbolCodegen)
  let args = [(ptr hmTy, "hashmap"), (ptr symbolTy, "symbol")]
  function "hashmap_contains" args i1 $ \[hm, symbolPtr] -> do
    bucketPtr <- bucketForHash hashFn hm symbolPtr
    loopEntriesInBucket symbolPtr bucketPtr $
      const $ ret (bit 1)

    ret $ bit 0

bucketForHash :: Operand -> Operand -> Operand -> IRCodegen Operand
bucketForHash hashFn hm symbolPtr = do
  h <- call hashFn [symbolPtr]
  idx <- modulo h capacity
  addr (bucketAt idx) hm

loopEntriesInBucket :: Operand -> Operand -> (Operand -> IRCodegen a) -> IRCodegen ()
loopEntriesInBucket symbolPtr bucketPtr instrsForMatch = do
  (vec, symbol) <- asks (vectorCodegen &&& symbolCodegen)

  entryCount <- call (Vector.vectorSize vec) [bucketPtr]

  loopFor (int32 0) (`ult` entryCount) (add (int32 1)) $ \i -> do
    entryPtr <- call (Vector.vectorGetValue vec) [bucketPtr, i]
    entrySymbolPtr <- addr symbolOf entryPtr

    isMatch <- call (Symbol.symbolIsEqual symbol) [entrySymbolPtr, symbolPtr]
    if' isMatch $
      instrsForMatch entryPtr

-- Helpers

-- NOTE: this assumes capacity is a power of 2!
-- TODO: add proper modulo instruction to llvm-codegen
modulo :: Operand -> Word32 -> IRCodegen Operand
modulo value divisor =
  value `and` int32 (toInteger divisor - 1)

loopBuckets :: Operand -> (Operand -> IRCodegen a) -> IRCodegen ()
loopBuckets hm f = do
  let currentCapacity = int32 $ toInteger capacity
  loopFor (int32 0) (`ult` currentCapacity) (add (int32 1)) $ \i -> do
    bucketPtr <- addr (bucketAt i) hm
    f bucketPtr

symbolType :: ModuleCodegen Type
symbolType =
  asks (Symbol.tySymbol . symbolCodegen)

data Index
  = HashMapIdx
  | BucketIdx  -- A vector inside the hashmap
  | EntryIdx
  | SymbolIdx
  | ValueIdx

bucketAt :: Operand -> Path 'HashMapIdx 'BucketIdx
bucketAt idx = mkPath [int32 0, idx]

symbolOf :: Path 'EntryIdx 'SymbolIdx
symbolOf = mkPath [int32 0]

valueOf :: Path 'EntryIdx 'ValueIdx
valueOf = mkPath [int32 1]

