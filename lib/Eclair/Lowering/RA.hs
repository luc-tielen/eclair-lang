{-# LANGUAGE RecursiveDo #-}

module Eclair.Lowering.RA ( compileLLVM ) where

import Protolude hiding (Type)
import Protolude.Unsafe (unsafeHead, unsafeFromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as TIO
import Data.Functor.Foldable
import Eclair.TypeSystem
import Eclair.RA.IR
import Eclair.RA.IndexSelection
import Eclair.Runtime.Store
import Eclair.Runtime.LLVM
import qualified Eclair.Runtime.BTree as BTree
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import LLVM.AST.Type hiding (Type)
import LLVM.AST.Name
import LLVM.Pretty
import Control.Monad.Morph (hoist)


type Module = ()  -- TODO llvm module type
-- TODO: rename
type FunctionsMap = Map Relation (Map Index Functions)
type StoreMap = Map Relation Store
-- TODO: add typeinfo to reader?
type CodegenM = IRBuilderT (ModuleBuilderT IO)

data LowerState
  = LowerState
  { endLabel :: Name
  , relations :: StoreMap
  , idxSelector :: IndexSelector
  }


compileLLVM :: TypeInfo -> RA -> IO Module
compileLLVM typeInfo ra = do
  let (indexMap, getIndexForSearch) = runIndexSelection ra
  moduleIR <- buildModuleT "module" $ do
    fnsMap <- generateFnsForRelations indexMap typeInfo

    let params = [(i32, "argc"), (ptr (ptr i8), "argv")]
    function "main" params i32 $ \[argc, argv] -> mdo
      storeMap <- allocateStores fnsMap
      let beginState = LowerState end storeMap getIndexForSearch
      runReaderT (generateProgramInstructions ra) beginState
      cleanupStores storeMap
      br end

      end <- block `named` "the.end"
      ret (int32 0)

  let output = ppllvm moduleIR
  TIO.putStrLn output -- TODO: remove

  -- TODO: generate code for reading/writing values from/to datalog

















allocateStores :: FunctionsMap -> CodegenM StoreMap
allocateStores = traverse $ \objMap ->
  map Store $ flip Map.traverseWithKey objMap $ \idx fns ->
    (, fns) <$> fnAllocateObj fns

cleanupStores :: StoreMap -> CodegenM ()
cleanupStores storeMap = do
  pure () -- TODO: cleanup each store

-- TODO: name
generateFnsForRelations :: IndexMap -> TypeInfo -> ModuleBuilderT IO FunctionsMap
generateFnsForRelations indexMap typeInfo = do
  results <- flip Map.traverseWithKey indexMap $ \r idxs -> do
    -- TODO: cache functions if possible?
    -- TODO: avoid codegen collisions between relations
    for (toList idxs) $ \idx -> do
      let meta = mkMeta idx (unsafeFromJust $ Map.lookup r typeInfo)
      (idx,) <$> BTree.codegen meta

  pure $ map Map.fromList results

generateProgramInstructions :: RA -> ReaderT LowerState CodegenM ()
generateProgramInstructions = cata $ \case
  -- TODO: compile to IR
  -- call into store functionality as much as possible,
  -- use index selector to compute indices
  -- use interpreter as guideline
  ModuleF actions -> sequence_ actions
  SearchF r alias clauses action -> do
    undefined
  ProjectF r values -> do
    undefined
  MergeF r1 r2 -> do
    store1 <- asks (storeForRelation r1)
    store2 <- asks (storeForRelation r2)
    -- TODO: move next code to store module so this focuses on actual lowering and not instructions?
    let objs1 = objects store1
        objs2 = objects store2
        objsCombined = Map.intersectionWith (,) objs1 objs2
    -- NOTE: fns1 == fns2
    -- obj1 = from/src, obj2 = to/dst
    for_ objsCombined $ \((obj1, fns1), (obj2, fns2)) -> do
      let begin = fnBegin fns1
          end = fnEnd fns1
          insertRange = fnInsertRange fns1
          allocateIter = fnAllocateIter fns1
      iterBegin1 <- allocateIter
      iterEnd1 <- allocateIter
      call begin [(obj1, []), (iterBegin1, [])]
      call end [(obj1, []), (iterEnd1, [])]
      call insertRange [(obj2, []), (iterBegin1, []), (iterEnd1, [])]
      pure ()
  PurgeF r -> do
    store <- asks (storeForRelation r)
    -- TODO: move next code to store module so this focuses on actual lowering and not instructions?
    for_ (objects store) $ \(obj, functions) -> do
      let purge = fnPurge functions
      call purge [(obj, [])]
  SwapF r1 r2 -> do
    store1 <- asks (storeForRelation r1)
    store2 <- asks (storeForRelation r2)
    -- TODO: move next code to store module so this focuses on actual lowering and not instructions?
    let objs1 = objects store1
        objs2 = objects store2
        objsCombined = Map.intersectionWith (,) objs1 objs2
    for_ objsCombined $ \((obj1, fns1), (obj2, fns2)) -> do
      let swap = fnSwap fns1  -- fns1 == fns2
      call swap [(obj1, []), (obj2, [])]
  ParF actions ->
    sequence_ actions -- TODO: make parallel
  LoopF actions -> mdo
    local (\s -> s { endLabel = end }) $
      loop $ sequence actions

    end <- block `named` "loop.end"
    pure ()
  ExitF rs -> do
    end <- asks endLabel
    kb <- asks relations
    let stores = mapMaybe (`Map.lookup` kb) rs
    flip cata (map objects stores) $ \case
      Nil -> pure ()
      Cons idxMap asm -> do
        let (obj, fns) = firstValueInMap idxMap
            isEmpty = fnIsEmpty fns
        condition <- call isEmpty [(obj, [])]
        if' isEmpty $
          br end

        asm
  _ -> undefined  -- TODO
  where
    storeForRelation r = unsafeFromJust . Map.lookup r . relations
    firstValueInMap m =
      let firstKey = unsafeHead $ Map.keys m
       in unsafeFromJust $ Map.lookup firstKey m

  {- TODO: how to handle the following?
  | Lit Number
  | ColumnIndex Relation ColumnIndex
  | Constrain RA RA  -- equality constraint
  | NotElem Relation [RA]
  -}

mkMeta :: Index -> [Type] -> BTree.Meta
mkMeta idx@(Index (SearchSignature columns)) ts =
  BTree.Meta
    { BTree.numColumns = length ts
    , BTree.index = columns
    , BTree.blockSize = 256
    , BTree.searchType = BTree.Linear
    }
