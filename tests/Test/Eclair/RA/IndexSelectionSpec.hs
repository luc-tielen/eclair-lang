module Test.Eclair.RA.IndexSelectionSpec
  ( module Test.Eclair.RA.IndexSelectionSpec
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec
import System.FilePath
import Eclair
import Eclair.Id
import Eclair.Parser
import Eclair.AST.Lower
import Eclair.RA.IndexSelection
import qualified Data.Text as T


idxSel :: FilePath -> IO IndexMap
idxSel path = do
  let file = "tests/fixtures" </> path <.> "dl"
  raResult <- compileRA file
  case raResult of
    Left err -> panic $ "Failed to parse " <> toText file <> "!"
    Right ra -> do
      let (indexMap, _) = runIndexSelection ra
      pure indexMap

resultsIn :: (Show a, Eq a) => IO a -> a -> IO ()
resultsIn action expected = do
  result <- action
  result `shouldBe` expected

toSelection :: [(T.Text, [[Column]])] -> IndexMap
toSelection info = idxMap
  where
    (texts, colss) = unzip info
    f text cols = (Id text, Set.fromList $ map Index cols)
    idxMap = Map.fromList $ zipWith f texts colss

spec :: Spec
spec = describe "Index selection" $ parallel $ do
  it "creates indexes for a single fact" $ do
    idxSel "single_fact" `resultsIn`
      toSelection [("another", [[0,1,2]]), ("edge", [[0,1]])]

  it "creates indexes for a single non-recursive rule" $ do
    idxSel "single_nonrecursive_rule" `resultsIn`
      toSelection [("edge", [[0,1]]), ("path", [[0,1]])]

  it "creates indexes for nested searches correctly" $ do
    idxSel "multiple_rule_clauses" `resultsIn`
      toSelection [ ("first",  [[0]])
                  , ("second", [[1,0]])
                  , ("third",  [[0,1]])
                  ]

  it "creates indexes for rules with equal columns correctly" $ do
    idxSel "rule_equal_columns" `resultsIn`
      toSelection [ ("a", [[0]])
                  , ("b", [[0,1]])
                  , ("c", [[0,1,2]])
                  , ("d", [[2,0,1,3]])
                  , ("other", [[0]])
                  ]

  it "handles multiple indexes on 1 rule correctly" $ do
    idxSel "index_selection" `resultsIn`
      toSelection [ ("a", [[0]])
                  , ("b", [[0]])
                  , ("c", [[0,1,2]])
                  , ("d", [[0]])
                  , ("triple", [[0,1,2], [2,0]])
                  ]

  it "selects a minimal set of indexes for a rule" $ do
    idxSel "minimal_index_selection" `resultsIn`
      toSelection [ ("first", [[1,0,2], [2,1]])
                  , ("second", [[0]])
                  , ("third", [[0]])
                  , ("fourth", [[0]])
                  , ("fifth", [[0]])
                  ]

  it "creates indexes for a rule with 2 clauses of same name" $ do
    idxSel "multiple_clauses_same_name" `resultsIn`
      toSelection [ ("link", [[0,1]])
                  , ("chain", [[0,1,2]])
                  ]

  it "creates indexes for a single recursive rule" $ do
    idxSel "single_recursive_rule" `resultsIn`
      toSelection [ ("delta_path", [[0,1]])
                  , ("new_path", [[0,1]])
                  , ("path", [[0,1]])
                  , ("edge", [[0,1]])
                  ]

  -- TODO variant where one is recursive

  it "creates indexes for mutually recursive rules" $ do
    idxSel "mutually_recursive_rules" `resultsIn`
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
    idxSel "index_for_chain" `resultsIn`
      toSelection [ ("a", [[0,1,2,3,4], [4,2,3]])
                  , ("b", [[0]])
                  ]
