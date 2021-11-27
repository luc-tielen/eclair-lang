module Eclair.RA.IndexSelection
  ( IndexMap
  , IndexSelector
  , Index(..)
  , SearchSignature(..)
  , Column
  , runIndexSelection
  ) where

-- Based on the paper "Automatic Index Selection for Large-Scale Datalog Computation"
-- http://www.vldb.org/pvldb/vol12/p141-subotic.pdf

import Protolude
import Protolude.Unsafe (unsafeHead, unsafeFromJust)
import Eclair.Syntax (startsWithIdPrefix, stripIdPrefixes)
import Eclair.RA.IR
import Algebra.Graph.Bipartite.AdjacencyMap
import Algebra.Graph.Bipartite.AdjacencyMap.Algorithm
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
  deriving (Eq, Ord, Show)

-- An index is also a search signature in essence, but a newtype for type safety
newtype Index = Index SearchSignature
  deriving (Eq, Ord, Show)

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
      indexSelection = Map.foldrWithKey (\r searchSet acc ->
        let graph = buildGraph searchSet
            matching = maxMatching graph
            chains = getChainsFromMatching graph matching
            indices = indicesFromChains searchSet chains
         in (r, indices):acc) mempty searchMap
      completeIndexSelection = mergeToMainRelation indexSelection
      combineIdxs idxs idx = idxs <> Set.singleton idx
      indexMap = foldl' combineIdxs mempty <$> Map.fromList completeIndexSelection
      indexSelector r s = unsafeFromJust $ do
        indexMapping <- snd <$> List.find ((== r) . fst) indexSelection
        Map.lookup s indexMapping
  in (indexMap, indexSelector)

-- combines delta_X, new_X into X
mergeToMainRelation :: IndexSelection -> IndexSelection
mergeToMainRelation idxSel
  = filter (startsWithIdPrefix . fst) idxSel
  & map (first stripIdPrefixes)
  & mappend idxSel

searchesForProgram :: RA -> SearchMap
searchesForProgram = zygo constraintsForSearch $ \case
  ProjectF r values ->
    let columns = take (length values) [0..]
        signature = SearchSignature $ Set.fromList columns
    in Map.singleton r (Set.singleton signature)
  SearchF r a (foldMap fst -> directCs) (nestedCs, sMap) ->
    let cs = directCs <> nestedCs
        relevantCols = mapMaybe (columnsForRelation a) cs
        signature = SearchSignature $ Set.fromList relevantCols
     in if null relevantCols
          then sMap
          else Map.insertWith (<>) r (Set.singleton signature) sMap
  ra -> Map.unionsWith (<>) (map snd ra)
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
  edges [(l, r) | l <- searches, r <- searches, l `isProperPrefixOf` r]
  where
    searches = toList searchSet
    isProperPrefixOf (SearchSignature xs) (SearchSignature ys) =
      -- Similar to isProperSubset for Set, but takes order into account;
      -- e.g. [1,2] is not a prefix of [0,1,2]
      xs /= ys && all identity (zipWith (==) (toList xs) (toList ys))

getChainsFromMatching :: SearchGraph -> SearchMatching -> Set SearchChain
getChainsFromMatching g m =
  let (covered, uncovered) = List.partition (`leftCovered` m) $ leftVertexList g
      uncoveredChains = map (:[]) uncovered
      coveredChains = map (getChain []) covered
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
        Nothing -> acc
        Just v ->
          -- Implicitly swap U and V side by passing in v as u:
          getChain (v:acc) v

indicesFromChains :: SearchSet -> Set SearchChain -> Map SearchSignature Index
indicesFromChains (Set.toList -> searchSet) (Set.toList -> chains) =
  -- TODO: use NonEmpty for safety here
  -- TODO: (flat)map over all chains? -> yes; or do reverse to take the longest?
  let indices = map unsafeHead chains
      mappings = [ (signature, Index idx) | signature <- searchSet
                 , idx <- indices
                 , signature `isPrefixOf` idx
                 ]
      isPrefixOf (SearchSignature xs) (SearchSignature ys) =
        -- Similar to isSubset for Set, but takes order into account;
        -- e.g. [1,2] is not a prefix of [0,1,2]
        all identity (zipWith (==) (toList xs) (toList ys))
   in Map.fromList mappings
