module Eclair.Runtime.Store
  ( Store(..)
  , Object
  , Functions(..)
  , mkObject
  , mkIter
  , mkValue
  , project
  , isEmpty
  , swap
  , merge
  , purge
  , destroy
  , lookupByIndex
  ) where

import Protolude hiding (Type, swap)
import Protolude.Unsafe (unsafeHead, unsafeFromJust)
import qualified Data.Map as Map
import LLVM.AST.Operand (Operand)
import Eclair.RA.IndexSelection
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import LLVM.AST.Type
import Control.Monad.Morph
import LLVM.IRBuilder.Combinators hiding (swap)

-- A stack allocated object.
type Object = Operand

-- Helper data type that manages multiple underlying datastructures.
-- This is useful for example when you need to update multiple indices,
-- but sometimes only want to query a specific one.
newtype Store
  = Store
  { objects :: Map Index (Object, Functions)  -- TODO: NonEmptyMap?
  }

-- Like a vtable in C++, except here everything is guaranteed to be inlined
-- because of specialization.
data Functions
  = Functions
  { fnDestroy :: Operand
  , fnPurge :: Operand
  , fnSwap :: Operand
  , fnBegin :: Operand
  , fnEnd :: Operand
  , fnIsEmpty :: Operand
  , fnLowerBound :: Operand
  , fnUpperBound :: Operand
  , fnContains :: Operand
  , fnInsert :: Operand
  , fnInsertRange :: Operand
  , fnIterIsEqual :: Operand
  , fnIterCurrent :: Operand
  , fnIterNext :: Operand
  , typeObj :: Type  -- TODO: try storing allocator iso type
  , typeIter :: Type
  , typeValue :: Type
  }

mkObject :: MonadIRBuilder m => Functions -> m Operand
mkObject fns = allocaTy (typeObj fns)

mkIter :: MonadIRBuilder m => Functions -> m Operand
mkIter fns = allocaTy (typeIter fns)

mkValue :: MonadIRBuilder m => Functions -> m Operand
mkValue fns = allocaTy (typeValue fns)

allocaTy :: MonadIRBuilder m => Type -> m Operand
allocaTy ty = alloca ty (Just (int32 1)) 0

destroy :: (MonadModuleBuilder m, MonadIRBuilder m)
        => Store -> m ()
destroy store =
  for_ (objects store) $ \(obj, fns) ->
    call (fnDestroy fns) [(obj, [])]

project :: (MonadModuleBuilder m, MonadIRBuilder m)
        => Store -> [Operand] -> m ()
project store vals = do
  -- NOTE: Value type is the same for all (but not the insert function)
  let (_ , fns) = unsafeHead $ Map.elems (objects store)
  value <- mkValue fns
  for_ (zip [0..] vals) $ \(i, val) ->
    assign (mkPath [int32 i]) value val

  for_ (objects store) $ \(obj, funcs) -> do
    call (fnInsert funcs) [(obj, []), (value, [])]

merge :: (MonadModuleBuilder m, MonadIRBuilder m, Monad m)
      => Store -> Store -> m ()
merge store1 store2 = do  -- NOTE: store1 = from/src, store2 = to/dst
  for_ (store1 `intersect` store2) $ \(obj1, obj2, fns) -> do
    let begin = fnBegin fns
        end = fnEnd fns
        insertRange = fnInsertRange fns
    iterBegin1 <- mkIter fns
    iterEnd1 <- mkIter fns
    call begin [(obj1, []), (iterBegin1, [])]
    call end [(obj1, []), (iterEnd1, [])]
    call insertRange [(obj2, []), (iterBegin1, []), (iterEnd1, [])]

purge :: (MonadModuleBuilder m, MonadIRBuilder m, Applicative m)
      => Store -> m ()
purge store =
  for_ (objects store) $ \(obj, functions) -> do
    call (fnPurge functions) [(obj, [])]

swap :: (MonadModuleBuilder m, MonadIRBuilder m, Applicative m)
     => Store -> Store -> m ()
swap store1 store2 = do
  for_ (store1 `intersect` store2) $ \(obj1, obj2, fns) -> do
    call (fnSwap fns) [(obj1, []), (obj2, [])]

isEmpty :: (MonadModuleBuilder m, MonadIRBuilder m)
        => Store -> m Operand
isEmpty store = call (fnIsEmpty fns) [(obj, [])]
  where
    (obj, fns) = firstValueInMap (objects store)
    firstValueInMap m = unsafeHead $ Map.elems m

-- NOTE: Only allowed if fns1 == fns2 (stores have similar structure)
intersect :: Store -> Store -> Map Index (Object, Object, Functions)
intersect store1 store2 =
  Map.intersectionWith f objs1 objs2
  where
    f (obj1, fns1) (obj2, _) = (obj1, obj2, fns1)
    objs1 = objects store1
    objs2 = objects store2

-- This should only be called internally by the compiler, so we know the index
-- passed in definitely exists.
lookupByIndex :: Store -> Index -> (Object, Functions)
lookupByIndex store idx =
  unsafeFromJust $ Map.lookup idx (objects store)
