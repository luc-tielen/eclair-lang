module Eclair.RA.IndexSelection ( getIndexForSearchInProgram ) where

-- Based on the paper "Automatic Index Selection for Large-Scale Datalog Computation"
-- http://www.vldb.org/pvldb/vol12/p141-subotic.pdf

import Protolude
import Protolude.Unsafe (unsafeHead, unsafeFromJust)
import Eclair.RA.IR
import Algebra.Graph.Bipartite.Undirected.AdjacencyMap
import Data.Functor.Foldable hiding (fold)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import qualified Data.Sequence as Seq
import qualified Data.List as List


type Column = Int

newtype SearchSignature
  = SearchSignature (Set Column)
  deriving (Eq, Ord)

-- An index is also a search signature in essence, but a newtype for type safety
newtype Index = Index SearchSignature
  deriving (Eq, Ord)

type SearchSet = Set SearchSignature
type SearchChain = [SearchSignature]  -- TODO: NonEmpty
type SearchMap = Map Relation SearchSet
type SearchGraph = AdjacencyMap SearchSignature SearchSignature
type SearchMatching = Matching SearchSignature SearchSignature
type IndexSelection = [(Relation, Map SearchSignature Index)]

getIndexForSearchInProgram :: RA -> (Relation -> SearchSignature -> Index)
getIndexForSearchInProgram ra r s =
  let searchMap = searchesForProgram ra
      indexSelection = Map.foldrWithKey (\r searchSet acc ->
        let graph = buildGraph searchSet
            matching = maximumMatching graph
            chains = getChainsFromMatching graph matching
            indices = indicesFromChains searchSet chains
         in (r, indices):acc) mempty searchMap
   in unsafeFromJust $ do
        indexMapping <- snd <$> List.find ((== r) . fst) indexSelection
        Map.lookup s indexMapping

searchesForProgram :: RA -> SearchMap
searchesForProgram = zygo constraintsForSearch $ \case
  SearchF r _ (foldMap fst -> cs) (snd -> sMap) ->
    let signature = SearchSignature $ Set.fromList $ mapMaybe (columnsForRelation r) cs
     in Map.insertWith (<>) r (Set.singleton signature) sMap
  ra ->
    foldr (Map.unionWith (<>) . snd) Map.empty ra
  where
    constraintsForSearch :: RAF [(Relation, Column)] -> [(Relation, Column)]
    constraintsForSearch = \case
      ColumnIndexF r col -> [(r, col)]
      e -> fold e
    columnsForRelation r (r' , col)
      | r == r'   = Just col
      | otherwise = Nothing

buildGraph :: SearchSet -> SearchGraph
buildGraph searchSet =
  vertices searches searches
  `overlay`
  edges [(l, r) | l <- searches, r <- searches, l `isSubset` r]
  where searches = Set.toList searchSet

isSubset :: SearchSignature -> SearchSignature -> Bool
isSubset (SearchSignature l) (SearchSignature r) =
  l `Set.isProperSubsetOf` r

getChainsFromMatching :: SearchGraph -> SearchMatching -> Set SearchChain
getChainsFromMatching g m =
  let (covered, uncovered) = List.partition (`leftCovered` m) $ leftVertexList g
      uncoveredChains = map (:[]) uncovered
      coveredChains = map (getChain []) covered
   in Set.fromList $ uncoveredChains <> coveredChains
  where
    -- getChain alternates between U and V side of the bipartite graph
    -- A lookup is done on V side:
    --   - if it finds no match, we have reached end of the chain
    --   - Otherwise, we found the next node in the chain, and use
    --     this node to find rest of the chain.
    getChain acc u =
      case Map.lookup u (pairOfLeft m) of
        Nothing -> acc
        Just v ->
          -- Implicitly swap U and V side by passing in v as u:
          getChain (v:acc) v

indicesFromChains :: SearchSet -> Set SearchChain -> Map SearchSignature Index
indicesFromChains (Set.toList -> searchSet) (Set.toList -> chains) =
  -- TODO: use NonEmpty for safety here
  let indices = map unsafeHead chains
      mappings = [ (signature, Index idx) | signature <- searchSet, idx <- indices
                 , signature `isSubset` idx
                 ]
   in Map.fromList mappings

-- Source code originally from: https://github.com/snowleopard/alga/pull/229/files
-- TODO: remove once it is merged into official alga upstream.

data Distance = Infinite | Finite Int
  deriving Eq

data Matching u v
  = Matching
  { pairOfLeft :: Map.Map u v
  , pairOfRight :: Map.Map v u
  }

data HKState s u v
  = HKS
  { distance :: STRef s (Map.Map u Int)
  , curMatching :: STRef s (Matching u v)
  , queue :: STRef s (Seq.Seq u)
  , visited :: STRef s (Set.Set u)
  }

maximumMatching :: forall u v. (Ord u, Ord v) => AdjacencyMap u v -> Matching u v
maximumMatching g = runST $ do
  dist <- newSTRef Map.empty
  m <- newSTRef $ Matching Map.empty Map.empty
  q <- newSTRef Seq.empty
  vis <- newSTRef Set.empty
  let state = HKS dist m q vis
  runHK state
  readSTRef m
  where
    runHK :: HKState s u v -> ST s ()
    runHK state = do
      writeSTRef (distance state) Map.empty
      run <- bfs g state
      when run $ do
        writeSTRef (visited state) Set.empty
        dfs g state
        runHK state

bfs :: forall s u v. (Ord u, Ord v) => AdjacencyMap u v -> HKState s u v -> ST s Bool
bfs g state = do
  traverse_ (enqueue state 0) . uncoveredVertices g =<< readSTRef (curMatching state)
  bfsLoop
  where
    bfsLoop = do
      mU <- dequeue state
      case mU of
        Just u -> do
          pathFoundForVertex <- bfsVertex u
          pathFoundForOtherVertices <- bfsLoop
          pure (pathFoundForVertex || pathFoundForOtherVertices)
        Nothing -> pure False

    bfsVertex :: u -> ST s Bool
    bfsVertex u = do
      Finite dist <- getDistance state u
      let d = dist + 1
      or <$> traverse (bfsEdge d) (neighbours g u)

    bfsEdge :: Int -> v -> ST s Bool
    bfsEdge d v = getMatch state v >>= \case
      Just u -> do
        dist <- getDistance state u
        when (dist == Infinite) $ enqueue state d u
        pure False
      Nothing -> pure True

dfs :: forall s u v. (Ord u, Ord v) => AdjacencyMap u v -> HKState s u v -> ST s ()
dfs g state =
  traverse_ (dfsVertex 0) . uncoveredVertices g =<< readSTRef (curMatching state)
  where
    dfsVertex :: Int -> u -> ST s Bool
    dfsVertex d u = do
      Finite dU <- getDistance state u
      vis <- readSTRef (visited state)
      if (dU == d + 1) && (u `Set.notMember` vis)
        then do
          modifySTRef (visited state) (Set.insert u)
          dfsEdges dU u (neighbours g u)
        else pure False

    dfsEdges :: Int -> u -> [v] -> ST s Bool
    dfsEdges d u = \case
      [] -> pure False
      (v:vs) -> getMatch state v >>= \case
        Nothing -> True <$ addEdge state u v
        Just u' -> do
          foundPath <- dfsVertex d u'
          if foundPath
            then True <$ addEdge state u v
            else dfsEdges d u vs

addEdge :: forall s u v. (Ord u, Ord v) => HKState s u v -> u -> v -> ST s ()
addEdge state u v =
  modifySTRef (curMatching state) addEdgeUnsafe
  where
    addEdgeUnsafe (Matching lr rl) =
      Matching (Map.insert u v lr) (Map.insert v u rl)

getDistance :: Ord u => HKState s u v -> u -> ST s Distance
getDistance state u = do
  distMap <- readSTRef (distance state)
  case Map.lookup u distMap of
    Nothing -> pure Infinite
    Just d -> pure $ Finite d

getMatch :: Ord v => HKState s u v -> v -> ST s (Maybe u)
getMatch state v = do
  m <- readSTRef (curMatching state)
  pure $ Map.lookup v (pairOfRight m)

neighbours :: Ord u => AdjacencyMap u v -> u -> [v]
neighbours g u =
  Set.toAscList $ unsafeFromJust $ Map.lookup u $ leftAdjacencyMap g

leftCovered :: Ord u => u -> Matching u v -> Bool
leftCovered u =
  Map.member u . pairOfLeft

uncoveredVertices :: Ord u => AdjacencyMap u v -> Matching u v -> [u]
uncoveredVertices g m =
  [u | u <- leftVertexList g, not (leftCovered u m)]

dequeue :: HKState s u v -> ST s (Maybe u)
dequeue state = do
  q <- readSTRef (queue state)
  case Seq.viewl q of
    a Seq.:< q' -> do
      writeSTRef (queue state) q'
      pure $ Just a
    Seq.EmptyL -> pure Nothing

enqueue :: Ord u => HKState s u v -> Int -> u -> ST s ()
enqueue state d u = do
  modifySTRef (distance state) (Map.insert u d)
  modifySTRef (queue state) (Seq.|> u)

-- End of maximumMatching related code (remove until here once when merged)
