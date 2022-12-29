{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.RA.IndexSelectionSpec
  ( module Test.Eclair.RA.IndexSelectionSpec
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec
import System.FilePath
import Eclair.Common.Id
import Eclair.Parser
import Eclair.AST.Lower
import Eclair.RA.IndexSelection
import Eclair.RA.Transforms
import qualified Eclair.TypeSystem as TS
import qualified Data.Text as T
import NeatInterpolation


idxSel :: FilePath -> Text -> IndexMap
idxSel path text' = do
  let file = "tests/fixtures" </> path <.> "dl"
      ast = (\(parsed, _, _, _) -> parsed) $ parseText file text'
   in case TS.typeCheck ast of
        Left _ -> panic $ "Failed to typecheck " <> toText file <> "!"
        Right typeInfo -> do
          let ra = simplify $ compileToRA ast
              (indexMap, _) = runIndexSelection (TS.infoTypedefs typeInfo) ra
           in indexMap

toSelection :: [(T.Text, [[Column]])] -> IndexMap
toSelection info = idxMap
  where
    (texts, colss) = unzip info
    f text' cols = (Id text', Set.fromList $ map Index cols)
    idxMap = Map.fromList $ zipWith f texts colss

spec :: Spec
spec = describe "Index selection" $ parallel $ do
  it "creates indexes for a single fact" $ do
    idxSel "single_fact" [text|
      @def edge(u32, u32).
      @def another(u32, u32, u32).

      edge(1, 2).
      edge(2, 3).

      another(1,2,3).
      |] `shouldBe`
      toSelection [("another", [[0,1,2]]), ("edge", [[0,1]])]

  it "creates indexes for a single non-recursive rule" $ do
    idxSel "single_nonrecursive_rule" [text|
      @def edge(u32, u32).
      @def path(u32, u32).

      edge(1,2).

      path(x,y) :- edge(x,y).
      |] `shouldBe`
      toSelection [("edge", [[0,1]]), ("path", [[0,1]])]

  it "creates indexes for nested searches correctly" $ do
    idxSel "multiple_rule_clauses" [text|
      @def first(u32).
      @def second(u32, u32).
      @def third(u32, u32).

      first(1).
      second(2, 3).

      third(x, y) :-
        first(y),
        second(x, y).
      |] `shouldBe`
      toSelection [ ("first",  [[0]])
                  , ("second", [[1,0]])
                  , ("third",  [[0,1]])
                  ]

  it "creates indexes for rules with equal columns correctly" $ do
    idxSel "rule_equal_columns" [text|
      @def a(u32).
      @def b(u32, u32).
      @def c(u32, u32, u32).
      @def d(u32, u32, u32, u32).
      @def other(u32).


      a(1).
      b(2, 3).
      c(4, 5, 6).
      d(7, 8, 9, 10).
      other(11).

      a(x) :-
        b(x, x),
        other(x).

      a(y) :-
        c(y, y, y),
        other(y).

      a(z) :-
        d(z, z, 12, z),
        other(z).
      |] `shouldBe`
      toSelection [ ("a", [[0]])
                  , ("b", [[0,1]])
                  , ("c", [[0,1,2]])
                  , ("d", [[2,0,1,3]])
                  , ("other", [[0]])
                  ]

  it "handles multiple indexes on 1 rule correctly" $ do
    idxSel "index_selection" [text|
      @def a(u32).
      @def b(u32).
      @def c(u32, u32, u32).
      @def d(u32).
      @def triple(u32, u32, u32).


      a(1).
      b(1).
      c(1, 2, 3).
      d(1).
      triple(4, 5, 6).

      a(y) :-
        // [2]
        triple(x, y, 123).

      b(x) :-
        // [0,1] => [0,1,2]
        triple(123, 456, x).

      c(x, y, z) :-
        // [0,1,2]
        triple(x, y, z).

      d(x) :-
        // [0, 2]
        triple(123, x, 456).
      |] `shouldBe`
      toSelection [ ("a", [[0]])
                  , ("b", [[0]])
                  , ("c", [[0,1,2]])
                  , ("d", [[0]])
                  , ("triple", [[0,1,2], [2,0]])
                  ]

  it "selects a minimal set of indexes for a rule" $ do
    idxSel "minimal_index_selection" [text|
      @def first(u32, u32, u32).
      @def second(u32).
      @def third(u32).
      @def fourth(u32).
      @def fifth(u32).

      // [1,0,2] ([0,1,2] re-ordered)
      first(1, 2, 3).
      second(1).
      third(1).
      fourth(1).
      fifth(1).

      second(x) :-
        // [0,1] => [1,0,2]
        first(123, 456, x).

      third(x) :-
        // [2,1]
        first(x, 123, 456).

      fourth(x) :-
        // [2] => [2,1]
        first(x, a, 123).

      fifth(x) :-
        // [1] => [1,0,2]
        first(x, 123, a).
      |] `shouldBe`
      toSelection [ ("first", [[1,0,2], [2,1]])
                  , ("second", [[0]])
                  , ("third", [[0]])
                  , ("fourth", [[0]])
                  , ("fifth", [[0]])
                  ]

  it "creates indexes for a rule with 2 clauses of same name" $ do
    idxSel "multiple_clauses_same_name" [text|
      @def link(u32, u32).
      @def chain(u32, u32, u32).

      link(1,2).

      chain(x, y, z) :-
        link(x, y),
        link(y, z).
      |] `shouldBe`
      toSelection [ ("link", [[0,1]])
                  , ("chain", [[0,1,2]])
                  ]

  it "creates indexes for a single recursive rule" $ do
    idxSel "single_recursive_rule" [text|
      @def edge(u32, u32).
      @def path(u32, u32).

      edge(1,2).

      path(x, y) :-
        edge(x, z),
        path(z, y).
      |] `shouldBe`
      toSelection [ ("delta_path", [[0,1]])
                  , ("new_path", [[0,1]])
                  , ("path", [[0,1]])
                  , ("edge", [[0,1]])
                  ]

  -- TODO variant where one is recursive

  it "creates indexes for mutually recursive rules" $ do
    idxSel "mutually_recursive_rules" [text|
      @def a(u32).
      @def b(u32).
      @def c(u32).
      @def d(u32).

      a(x) :- b(x), c(x).
      b(1).
      b(x) :- c(x), d(x).
      c(2).
      c(x) :- b(x), d(x).
      d(3).
      |] `shouldBe`
      toSelection [ ("a", [[0]])
                  , ("b", [[0]])
                  , ("new_b", [[0]])
                  , ("delta_b", [[0]])
                  , ("c", [[0]])
                  , ("new_c", [[0]])
                  , ("delta_c", [[0]])
                  , ("d", [[0]])
                  ]

  -- TODO tests for rules with >2 clauses, ...

  it "calculates index correctly for multiple columns at once" $ do
    idxSel "index_for_chain" [text|
      @def a(u32, u32, u32, u32, u32).
      @def b(u32).


      a(1,2,3,4,5).
      b(1).

      b(x) :-
        // [0,1]
        a(123, 123, x, y, z).

      b(x) :-
        // [2,3,4]
        a(x, y, 123, 123, 123).

      b(x) :-
        // [4] => [4,2,3]
        a(x, y, z, a, 123).
      |] `shouldBe`
      toSelection [ ("a", [[0,1,2,3,4], [4,2,3]])
                  , ("b", [[0]])
                  ]

  it "calculates indexes correctly for programs with no top level facts" $ do
    idxSel "no_top_level_facts" [text|
      @def edge(u32, u32).
      @def path(u32, u32).

      path(x, y) :-
        edge(x, y).

      path(x, z) :-
        edge(x, y),
        path(y, z).
      |] `shouldBe`
      toSelection [ ("delta_path", [[0,1]])
                  , ("new_path", [[0,1]])
                  , ("path", [[0,1]])
                  , ("edge", [[0,1]])
                  ]

  it "does not use 'NoElem' constraints to compute indexes" $ do
    idxSel "index_selection_should_only_check_for_equalities" [text|
      @def edge(u32, u32).
      @def reachable(u32, u32).

      reachable(x, y) :-
        edge(x, y).

      reachable(x, z) :-
        edge(x, _),
        reachable(_, z).
      |] `shouldBe`
      toSelection [ ("delta_reachable", [[0,1]])
                  , ("new_reachable", [[0,1]])
                  , ("reachable", [[0,1]])
                  , ("edge", [[0,1]])
                  ]
