module Eclair.RA.IndexSelection
  ( IndexMap
  , IndexSelector
  , Index(..)
  , SearchSignature(..)
  , Column
  , runIndexSelection
  , columnsForRelation
  , NormalizedEquality(..)
  , normalizedEqToConstraints
  , extractEqualities
  , definedColumnsFor
  ) where

-- Based on the paper "Automatic Index Selection for Large-Scale Datalog Computation"
-- http://www.vldb.org/pvldb/vol12/p141-subotic.pdf

import Data.Maybe (fromJust)
import Eclair.Common.Id
import Eclair.Common.Pretty
import Eclair.RA.IR
import Eclair.Comonads
import Eclair.TypeSystem (TypedefInfo)
import Algebra.Graph.Bipartite.AdjacencyMap
import Algebra.Graph.Bipartite.AdjacencyMap.Algorithm hiding (matching)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.DList.DNonEmpty as NE


type Column = Int

newtype SearchSignature
  = SearchSignature (Set Column)
  deriving (Eq, Ord, Show)

newtype Index
  = Index
  { unIndex :: [Column]  -- TODO: use NonEmpty
  } deriving (Eq, Ord, Show)

instance Pretty Index where
  pretty (Index columns) =
    brackets $ withCommas $ map pretty columns

type SearchSet = Set SearchSignature
type SearchChain = NonEmpty SearchSignature
type SearchMap = Map Relation SearchSet
type SearchGraph = AdjacencyMap SearchSignature SearchSignature
type SearchMatching = Matching SearchSignature SearchSignature
type IndexSelection = [(Relation, Map SearchSignature Index)]

type IndexMap = Map Relation (Set Index)
type IndexSelector = Relation -> SearchSignature -> Index

runIndexSelection :: TypedefInfo -> RA -> (IndexMap, IndexSelector)
runIndexSelection defInfo ra =
  let searchMap = searchesForProgram defInfo ra
      indexSelection :: IndexSelection
      indexSelection = Map.foldrWithKey (\r searchSet acc ->
        let graph = buildGraph searchSet
            matching = maxMatching graph
            chains = getChainsFromMatching graph matching
            indices = indicesFromChains searchSet chains
         in (r, indices):acc) mempty searchMap
      combineIdxs idxs idx = idxs <> one idx
      indexMap = foldl' combineIdxs mempty <$> Map.fromList indexSelection
      indexSelector r s = fromJust $ do
        indexMapping <- snd <$> List.find ((== r) . fst) indexSelection
        Map.lookup s indexMapping
  in (indexMap, indexSelector)

data SearchFact
  = SearchOn Relation SearchSignature
  | Related Relation Relation
  deriving (Eq, Ord)

-- All relations (including delta_XXX, new_XXX) need atleast one index for full order search
-- Swap operation requires indices of r1 and r2 to be related.
searchesForProgram :: TypedefInfo -> RA -> SearchMap
searchesForProgram defInfo ra =
  let raSearchFacts = execState (gcata (dsitribute extractEqualities) constraintsForRA ra) mempty
      relationFullSearches = getFullSearchesForRelations defInfo
      facts = raSearchFacts <> relationFullSearches
   in solve facts
  where
    addFact fact = modify (fact:)
    constraintsForRA = \case
      SearchF _ r a (foldMap tSnd -> eqs) (tThd -> action) -> do
        -- 1. Only direct constraints in the search matter, since that is what
        --    is used to select the index with, afterwards we are already
        --    looping over the value!
        -- 2. Only equality constraints matter, not "NoElem" constraints!
        let cs = concatMap normalizedEqToConstraints eqs
            relevantCols = mapMaybe (columnsForRelation a) cs
            signature = SearchSignature $ Set.fromList relevantCols
        unless (null relevantCols) $ do
          addFact $ SearchOn r signature
        action
      NotElemF _ r cols -> do
        let values = map tFst cols
            cs = definedColumnsFor values
            signature = SearchSignature $ Set.fromList cs
        addFact $ SearchOn r signature
      MergeF _ from' _ -> do
        -- Always add a full search signature for the from relation, so we don't lose any data.
        let columns = columnsFor . fromJust $ Map.lookup (stripIdPrefixes from') defInfo
            signature = SearchSignature $ Set.fromList columns
        addFact $ SearchOn from' signature
      SwapF _ r1 r2  ->
        addFact $ Related r1 r2
      raf ->
        traverse_ tThd raf

    dsitribute
      :: Corecursive t
      => (Base t (t, b) -> b)
      -> (Base t (Triple t b a) -> Triple t b (Base t a))
    dsitribute g m =
      let base_t_t = map tFst m
          base_t_tb = map (tFst &&& tSnd) m
          base_t_a = map tThd m
        in Triple (embed base_t_t) (g base_t_tb) base_t_a

-- For every relation we add atleast 1 one index with all columns.
getFullSearchesForRelations :: TypedefInfo -> [SearchFact]
getFullSearchesForRelations defInfo =
  [ SearchOn r . toSearchSignature $ unsafeLookup r
  | r <- Map.keys defInfo
  ]
  where
    unsafeLookup r = fromJust $ Map.lookup r defInfo
    toSearchSignature = SearchSignature . Set.fromList . columnsFor

data NormalizedEquality
  = NormalizedEquality Alias Column RA
  deriving Show

extractEqualities :: RAF (RA, [NormalizedEquality]) -> [NormalizedEquality]
extractEqualities = \case
  CompareOpF _ Equals (lhs, _) (rhs, _) | isDefined lhs && isDefined rhs ->
    toEqualities lhs rhs
  raf ->
    foldMap snd raf
  where
    isDefined = \case
      Undef {} -> False
      _ -> True
    toEqualities lhs rhs = case (lhs, rhs) of
      (ColumnIndex _ lA lCol, ColumnIndex _ rA rCol) ->
        [ NormalizedEquality lA lCol rhs
        , NormalizedEquality rA rCol lhs
        ]
      (ColumnIndex _ lA lCol, _) ->
        [NormalizedEquality lA lCol rhs]
      (_, ColumnIndex {}) ->
        toEqualities rhs lhs
      _ ->
        mempty

normalizedEqToConstraints :: NormalizedEquality -> [(Relation, Column)]
normalizedEqToConstraints = \case
  NormalizedEquality a1 c1 ra -> case ra of
    ColumnIndex _ a2 c2 ->
      [(a1, c1), (a2, c2)]
    _ ->
      [(a1, c1)]

columnsForRelation :: Relation -> (Relation, Column) -> Maybe Column
columnsForRelation r (r', col)
  | r == r'   = Just col
  | otherwise = Nothing

solve :: [SearchFact] -> SearchMap
solve facts = execState (traverse solveOne $ sort facts) mempty
  where
    solveOne = \case
      SearchOn r signature ->
        modify (Map.insertWith (<>) r (one signature))
      Related r1 r2 -> do
        signatures1 <- gets (Map.findWithDefault mempty r1)
        signatures2 <- gets (Map.findWithDefault mempty r2)
        let combined = signatures1 <> signatures2
        modify $ Map.insert r1 combined
               . Map.insert r2 combined

buildGraph :: SearchSet -> SearchGraph
buildGraph searchSet =
  vertices searches searches
  `overlay`
  edges [(l, r) | l <- searches, r <- searches, l `isSubsetOf` r]
  where
    searches = toList searchSet
    isSubsetOf (SearchSignature xs) (SearchSignature ys) =
      xs `Set.isProperSubsetOf` ys

getChainsFromMatching :: SearchGraph -> SearchMatching -> Set SearchChain
getChainsFromMatching g m =
  let (covered, uncovered) = List.partition (`leftCovered` m) $ leftVertexList g
      uncoveredChains = map one uncovered
      coveredChains = map (\n -> NE.toNonEmpty $ getChain (pure n) n) covered
   in Set.fromList $ uncoveredChains <> coveredChains
  where
    leftCovered :: Ord a => a -> Matching a b -> Bool
    leftCovered a = Map.member a . pairOfLeft
    -- getChain alternates between U and V side of the bipartite graph
    -- A lookup is done on V side:
    --   - if it finds no match, we have reached end of the chain
    --   - Otherwise, we found the next node in the chain, and use
    --     this node to find rest of the chain.
    getChain acc u =
      case Map.lookup u (pairOfLeft m) of
        Nothing ->
          -- Longest chain at end, needed in indexForChain
          acc
        Just v ->
          -- Implicitly swap U and V side by passing in v as u:
          getChain (NE.snoc acc v) v

indicesFromChains :: SearchSet -> Set SearchChain -> Map SearchSignature Index
indicesFromChains (Set.toList -> searchSet) (Set.toList -> chains) =
  Map.fromList [ (signature, indexForChain chain)
               | chain <- chains
               , signature <- searchSet
               , signature `elem` chain
               ]

-- NOTE: assumes chain is sorted from shortest to longest
indexForChain :: SearchChain -> Index
indexForChain chain = Index $ foldMap Set.toList columns
  where
    SearchSignature shortest :| rest = chain
    diffColumns = zipWith columnDiff rest (toList chain)
    columns = shortest : diffColumns
    columnDiff (SearchSignature long) (SearchSignature short) =
      long Set.\\ short

columnsFor :: [a] -> [Int]
columnsFor = zipWith const [0..]

definedColumnsFor :: [RA] -> [Int]
definedColumnsFor values =
  catMaybes $ zipWith f [0..] values
  where
    f c = \case
      Undef {} -> Nothing
      _ -> Just c
