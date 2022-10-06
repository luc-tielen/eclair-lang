module Eclair.RA.IndexSelection
  ( IndexMap
  , IndexSelector
  , Index(..)
  , SearchSignature(..)
  , Column
  , runIndexSelection
  , columnsForRelation
  , NormalizedEquality(..)
  , Val(..)
  , normalizedEqToConstraints
  , extractEqualities
  ) where

-- Based on the paper "Automatic Index Selection for Large-Scale Datalog Computation"
-- http://www.vldb.org/pvldb/vol12/p141-subotic.pdf

import Data.Maybe (fromJust)
import Eclair.RA.IR
import Eclair.Pretty
import Eclair.Comonads
import Eclair.TypeSystem (TypeInfo)
import Algebra.Graph.Bipartite.AdjacencyMap
import Algebra.Graph.Bipartite.AdjacencyMap.Algorithm hiding (matching)
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

runIndexSelection :: TypeInfo -> RA -> (IndexMap, IndexSelector)
runIndexSelection typeInfo ra =
  let searchMap = searchesForProgram typeInfo ra
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

searchesForProgram :: TypeInfo -> RA -> SearchMap
searchesForProgram typeInfo ra =
  let searchFacts = execState (gcata (dsitribute extractEqualities) constraintsForRA ra) mempty
      facts = searchFacts ++ getAdditionalSearchFacts typeInfo searchFacts
   in solve facts
  where
    addFact fact = modify (fact:)
    constraintsForRA = \case
      ProjectF r values -> do
        -- TODO: handle this with type declaration instead
        let columns = columnsFor values
            signature = SearchSignature $ Set.fromList columns
        addFact $ SearchOn r signature
      SearchF r a (traverse_ tSnd -> m) (tThd -> action) -> do
        -- 1. Only direct constraints in the search matter, since that is what
        --    is used to select the index with, afterwards we are already
        --    looping over the value!
        -- 2. Only equality constraints matter, not "NoElem" constraints!
        let eqs = execWriter m
            cs = concatMap normalizedEqToConstraints eqs
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
      raf -> traverse_ tThd raf

    dsitribute
      :: Corecursive t
      => (Base t (t, b) -> b)
      -> (Base t (Triple t b a) -> Triple t b (Base t a))
    dsitribute g m =
      let base_t_t = map tFst m
          base_t_tb = map (tFst &&& tSnd) m
          base_t_a = map tThd m
        in Triple (embed base_t_t) (g base_t_tb) base_t_a


-- Finds all facts that didn't have any searches,
-- and defaults those to a search that makes use of all columns.
getAdditionalSearchFacts :: TypeInfo -> [SearchFact] -> [SearchFact]
getAdditionalSearchFacts typeInfo searchFacts =
  [ SearchOn r . toSearchSignature $ unsafeLookup r
  | r <- Map.keys typeInfo
  , r `notElem` rs
  ]
  where
    rs = mapMaybe searchedOnRelation searchFacts
    unsafeLookup r = fromJust $ Map.lookup r typeInfo
    toSearchSignature = SearchSignature . Set.fromList . columnsFor
    searchedOnRelation = \case
      SearchOn r _ -> Just r
      _ -> Nothing

-- TODO better name
data Val
  = AliasVal Alias Column
  | Constant Word32
  deriving Show

data NormalizedEquality
  = Equality Alias Column Val
  deriving Show

extractEqualities :: RAF (RA, Writer [NormalizedEquality] ()) -> Writer [NormalizedEquality] ()
extractEqualities = \case
  ConstrainF (lhs, _) (rhs, _) -> do
    case (lhs, rhs) of
      (ColumnIndex lA lCol, ColumnIndex rA rCol) ->
        tell [ Equality lA lCol (AliasVal rA rCol)
             , Equality rA rCol (AliasVal lA lCol)
             ]
      (ColumnIndex lA lCol, Lit r) ->
        tell [Equality lA lCol (Constant r)]
      (Lit l, ColumnIndex rA rCol) ->
        tell [Equality rA rCol (Constant l)]
      _ ->
        pass
  raf ->
    traverse_ snd raf

normalizedEqToConstraints :: NormalizedEquality -> [(Relation, Column)]
normalizedEqToConstraints = \case
  Equality a1 c1 (AliasVal a2 c2) ->
    [(a1, c1), (a2, c2)]
  Equality a1 c1 _ ->
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
