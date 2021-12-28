{-# LANGUAGE RecursiveDo #-}

module Eclair.Lowering.RA ( compileLLVM ) where

import Protolude hiding (Type, bit, not, and)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))
import Control.Monad.Reader
import Data.Functor.Foldable
import Data.List ((!!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as TIO
import Eclair.RA.IR
import Eclair.RA.IndexSelection
import Eclair.Runtime.Store (Store(..), Object, Functions(..))
import Eclair.TypeSystem
import qualified Eclair.Runtime.BTree as BTree
import qualified Eclair.Runtime.Store as Store
import LLVM.AST.Name
import LLVM.AST.Operand hiding (local)
import LLVM.AST.Type hiding (Type)
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Combinators
import LLVM.Pretty
import qualified LLVM.AST.IntegerPredicate as IP


type Module = ()  -- TODO llvm module type
-- TODO: rename
type FunctionsMap = Map Relation (Map Index Functions)
type StoreMap = Map Relation Store
-- TODO: add typeinfo to reader?
type CodegenM = IRBuilderT (ModuleBuilderT IO)

type Value = [Operand]  -- TODO: remove?

data LowerState
  = LowerState
  { endLabel :: Name
  , typeEnv :: TypeInfo
  , relations :: StoreMap
  , idxSelector :: IndexSelector
  , aliasMap :: Map Alias Operand  -- Maps to a value pointer
  }

data QueryState
  = SearchState Alias Operand LowerState
  | ProjectState Relation LowerState

type QueryM = ReaderT QueryState CodegenM


compileLLVM :: TypeInfo -> RA -> IO Module
compileLLVM typeInfo ra = pure ()
  {-
  TODO finish remaining code
do
  let (indexMap, getIndexForSearch) = runIndexSelection ra
  moduleIR <- buildModuleT "module" $ do
    fnsMap <- generateFnsForRelations indexMap typeInfo

    let params = [(i32, "argc"), (ptr (ptr i8), "argv")]
    -- TODO: split into multiple functions
    function "main" params i32 $ \[argc, argv] -> mdo
      storeMap <- allocateStores fnsMap
      let beginState = LowerState end typeInfo storeMap getIndexForSearch mempty
      runReaderT (generateProgramInstructions ra) beginState
      cleanupStores storeMap
      br end

      end <- block `named` "the.end"
      ret (int32 0)

  let output = ppllvm moduleIR
  TIO.putStrLn output -- TODO: remove
-}
  -- TODO: generate code for reading/writing values from/to datalog

allocateStores :: FunctionsMap -> CodegenM StoreMap
allocateStores = traverse $ \objMap ->
  map Store $ flip Map.traverseWithKey objMap $ \idx fns ->
    (, fns) <$> Store.mkObject fns
    -- TODO: call initialize (via Store)?

cleanupStores :: StoreMap -> CodegenM ()
cleanupStores storeMap =
  for_ storeMap Store.destroy

-- TODO: name
generateFnsForRelations :: IndexMap -> TypeInfo -> ModuleBuilderT IO FunctionsMap
generateFnsForRelations indexMap typeInfo = do
  results <- flip Map.traverseWithKey indexMap $ \r idxs -> do
    -- TODO: cache functions if possible?
    -- TODO: avoid codegen collisions between relations
    for (toList idxs) $ \idx -> do
      let meta = mkMeta idx (fromJust $ Map.lookup r typeInfo)
      (idx,) <$> BTree.codegen meta

  pure $ map Map.fromList results

data Bound
  = LowerBound
  | UpperBound

-- TODO: finish implementation, broken atm
initValue :: Bound -> Functions -> [Column] -> ReaderT LowerState CodegenM Operand
initValue bound fns columns = do
  typeInfo <- asks typeEnv
  value <- Store.mkValue fns
  let columnNrs = take (length typeInfo) [0..]
      valuesWithCols = [(nr, x) | nr <- columnNrs, let x = if nr `elem` columns
                                                             then bounded
                                                             else dontCare]
  for_ valuesWithCols $ \(i, val) -> do
    -- TODO: take actual values into account!
    assign (mkPath [int32 (toInteger i)]) value val
  pure value
  where
    -- NOTE: only supports unsigned integers for now!
    bounded = case bound of
      LowerBound -> int32 0
      UpperBound -> int32 0xffffffff
    dontCare = int32 0

generateProgramInstructions :: RA -> ReaderT LowerState CodegenM ()
generateProgramInstructions = zygo extractQueryInfo $ \case
  ModuleF actions ->
    traverse_ snd actions
  SearchF r alias (unzip . map fst -> (qs, constraints)) (snd -> asm) -> do
    store <- asks (storeForRelation r)
    -- TODO: refactor/move to Store.hs
    (idx, columns) <- idxFromConstraints r (concat constraints)
    let (obj, fns) = Store.lookupByIndex store idx
        query = andM identity qs
    lbValue <- initValue LowerBound fns columns
    ubValue <- initValue UpperBound fns columns
    beginIter <- Store.mkIter fns
    endIter <- Store.mkIter fns
    call (fnLowerBound fns) [(obj, []), (lbValue, []), (beginIter, [])]
    call (fnUpperBound fns) [(obj, []), (ubValue, []), (endIter, [])]
    let hasNext = do
          isEqual <- call (fnIterIsEqual fns) [(beginIter, []), (endIter, [])]
          not' isEqual
    loopWhile hasNext $ mdo
      currValue <- call (fnIterCurrent fns) [(beginIter, [])]
      matchesQuery <- withReaderT (SearchState alias currValue) query
      if' matchesQuery $
        updateAlias fns alias currValue
        asm

      call (fnIterNext fns) [(beginIter, [])]
  ProjectF r (map (fst . fst) -> unresolvedValues) -> do
    store <- asks (storeForRelation r)
    values <- withReaderT (ProjectState r) $ sequence unresolvedValues
    Store.project store values
  MergeF r1 r2 -> do
    store1 <- asks (storeForRelation r1)
    store2 <- asks (storeForRelation r2)
    Store.merge store1 store2
  PurgeF r -> do
    store <- asks (storeForRelation r)
    Store.purge store
  SwapF r1 r2 -> do
    store1 <- asks (storeForRelation r1)
    store2 <- asks (storeForRelation r2)
    Store.swap store1 store2
  ParF actions ->
    traverse_ snd actions -- TODO: make parallel
  LoopF actions -> mdo
    local (\s -> s { endLabel = end }) $
      loop $ traverse_ snd actions

    end <- block `named` "loop.end"
    pure ()
  ExitF rs -> do
    end <- asks endLabel
    kb <- asks relations
    let stores = mapMaybe (`Map.lookup` kb) rs
    allEmpty <- andM Store.isEmpty stores
    if' allEmpty $
      br end
  _ -> panic "Unexpected variant when lowering RA IR to LLVM."

idxFromConstraints :: Relation -> [(Relation, Column)] -> ReaderT LowerState CodegenM (Index, [Column])
idxFromConstraints r constraints = do
    getIndexForSearch <- asks idxSelector
    let columns = mapMaybe (columnsForRelation r) constraints
        signature = SearchSignature $ Set.fromList columns
        idx = getIndexForSearch r signature
    pure (idx, columns)


-- TODO: is constraintsForSearch enough, maybe we need constraintsForRA too?
extractQueryInfo :: RAF (QueryM Operand, [(Relation, Column)])
                 ->     (QueryM Operand, [(Relation, Column)])
extractQueryInfo = toQuery `combine` constraintsForSearch where
  -- NOTE: The following typechecks, but actually returns completely
  -- different things on the LLVM level because there it is all "just Operands".
  toQuery :: RAF (QueryM Operand) -> QueryM Operand
  toQuery = \case
    LitF x -> pure (int32 $ toInteger x)
    ColumnIndexF a' col -> ask >>= \case
      SearchState a value ls ->
        if a == a'
          then getColumn value col
          else do
            let currentAliasValue = lookupAliasValue a ls
            getColumn currentAliasValue col
      ProjectState r ls -> do
        let currentAliasValue = lookupAliasValue r ls
        getColumn currentAliasValue col
      where
        getColumn value col =
          deref (mkPath [int32 $ toInteger col]) value
    ConstrainF lhs rhs -> do
      a <- lhs
      b <- rhs
      a `eq` b
    NotElemF r values -> do
      store <- asks (storeForRelation r . lowerState)
      let idx = mkFindIndex values
          (obj, fns) = fromJust $ Map.lookup idx (Store.objects store)
      vals <- sequence values
      value <- Store.mkValue fns
      for_ (zip [0..] vals) $ \(i, val) ->
        assign (mkPath [int32 i]) value val
      not' =<< call (fnContains fns) [(obj, []), (value, [])]
    _ ->
      panic "Unsupported variant in extractQueryInfo when lowering RA IR to LLVM."
  lowerState = \case
    SearchState _ _ ls -> ls
    ProjectState _ ls -> ls

combine :: Functor f => (f a -> a) -> (f b -> b) -> f (a, b) -> (a, b)
combine f g = f . map fst &&& g . map snd

andM :: (MonadModuleBuilder m, MonadIRBuilder m)
     => (a -> m Operand) -> [a] -> m Operand
andM f as = flip cata as $ \case
  Nil -> pure (bit 1)
  Cons a asm -> do
    result <- f a
    (result `and`) =<< asm

storeForRelation :: Relation -> LowerState -> Store
storeForRelation r =
  fromJust . Map.lookup r . relations

lookupAliasValue :: Alias -> LowerState -> Operand
lookupAliasValue a =
  fromJust . Map.lookup a . aliasMap

updateAlias :: Functions -> Alias -> Operand -> ReaderT LowerState CodegenM () -> ReaderT LowerState CodegenM ()
updateAlias fns alias iter m = do
  curr <- call (fnIterCurrent fns) [(iter, [])]
  local (\ls -> ls { aliasMap = Map.insert alias curr (aliasMap ls) }) m

-- TODO: Is this index always correct? -> no; how to get corresponding index?
mkFindIndex :: [a] -> Index
mkFindIndex values =
  Index $ zipWith const [0..] values

mkMeta :: Index -> [Type] -> BTree.Meta
mkMeta idx@(Index columns) ts =
  BTree.Meta
    { BTree.numColumns = length ts
    , BTree.index = columns
    , BTree.blockSize = 256
    , BTree.searchType = BTree.Linear
    }
