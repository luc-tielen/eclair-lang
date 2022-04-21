module Eclair.RA.IndexSelection
  ( IndexMap
  , IndexSelector
  , Index(..)
  , SearchSignature(..)
  , Column
  , runIndexSelection
  , columnsForRelation
  , constraintsForSearch
  ) where

-- Based on the paper "Automatic Index Selection for Large-Scale Datalog Computation"
-- http://www.vldb.org/pvldb/vol12/p141-subotic.pdf

import Data.Maybe (fromJust)
import Eclair.RA.IR
import Eclair.Pretty
import Algebra.Graph.Bipartite.AdjacencyMap
import Algebra.Graph.Bipartite.AdjacencyMap.Algorithm hiding (matching)
import Data.Functor.Foldable hiding (fold)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List


type Column = Int

newtype SearchSignature
  = SearchSignature (Set Column)
  deriving (Eq, Ord, Show)

newtype Index = Index [Column]  -- TODO: use NonEmpty
  deriving (Eq, Ord, Show)

instance Pretty Index where
  pretty (Index columns) =
    brackets $ withCommas $ map pretty columns

type SearchSet = Set SearchSignature
type SearchChain = [SearchSignature]  -- TODO: NonEmpty
type SearchMap = Map Relation SearchSet
type SearchGraph = AdjacencyMap SearchSignature SearchSignature
type SearchMatching = Matching SearchSignature SearchSignature
type IndexSelection = [(Relation, Map SearchSignature Index)]

type IndexMap = Map Relation (Set Index)
type IndexSelector = Relation -> SearchSignature -> Index

runIndexSelection :: RA -> (IndexMap, IndexSelector)
runIndexSelection ra =
  let searchMap = searchesForProgram ra
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

searchesForProgram :: RA -> SearchMap
searchesForProgram ra = solve $ execState (zygo constraintsForSearch constraintsForRA ra) mempty
  where
    addFact fact = modify (fact:)
    constraintsForRA = \case
      ProjectF r values -> do
        -- TODO: handle this with type declaration instead
        let columns = columnsFor values
            signature = SearchSignature $ Set.fromList columns
        addFact $ SearchOn r signature
      SearchF r a (foldMap fst -> cs) (snd -> action) -> do
        -- Only direct constraints in the search matter, since that is what
        -- is used to select the index with, afterwards we are already
        -- looping over the value!
        let relevantCols = mapMaybe (columnsForRelation a) cs
            signature = SearchSignature $ Set.fromList relevantCols
        unless (null relevantCols) $ do
          addFact $ SearchOn r signature
        action
      NotElemF r cols -> do
        let cs = columnsFor cols
            signature = SearchSignature $ Set.fromList cs
        addFact $ SearchOn r signature
      MergeF r1 r2 -> addFact $ Related r1 r2
      SwapF r1 r2  -> addFact $ Related r1 r2
      raf -> traverse_ snd raf

constraintsForSearch :: RAF [(Relation, Column)] -> [(Relation, Column)]
constraintsForSearch = \case
  ColumnIndexF r col -> [(r, col)]
  e -> fold e

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
      coveredChains = map (\n -> getChain [n] n) covered
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
          -- TODO difflist for performance?
          -- Longest chain at end, needed in indexForChain
          reverse acc
        Just v ->
          -- Implicitly swap U and V side by passing in v as u:
          getChain (v:acc) v

indicesFromChains :: SearchSet -> Set SearchChain -> Map SearchSignature Index
indicesFromChains (Set.toList -> searchSet) (Set.toList -> chains) =
  Map.fromList [ (signature, indexForChain chain)
               | chain <- chains
               , signature <- searchSet
               , signature `elem` chain
               ]

-- TODO: use NonEmpty for safety here
-- NOTE: assumes chain is sorted from shortest to longest
indexForChain :: SearchChain -> Index
indexForChain chain = Index $ foldMap Set.toList columns
  where
    SearchSignature shortest : rest = chain
    diffColumns = zipWith columnDiff rest chain
    columns = shortest : diffColumns
    columnDiff (SearchSignature long) (SearchSignature short) =
      long Set.\\ short

columnsFor :: [a] -> [Int]
columnsFor = zipWith const [0..]
