module Test.Eclair.RA.IndexSelectionSpec
  ( module Test.Eclair.RA.IndexSelectionSpec
  ) where

import Protolude hiding ((<.>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec
import System.FilePath
import Eclair.Syntax
import Eclair.Parser
import Eclair.Lowering.AST
import Eclair.RA.IndexSelection
import qualified Data.Text as T


idxSel :: FilePath -> IO IndexMap
idxSel path = do
  let file = "tests/fixtures/codegen" </> path <.> "dl"
  parseResult <- parseFile file
  case parseResult of
    Left err -> panic $ "Failed to parse " <> T.pack file <> "!"
    Right ast -> do
      let ra = compileRA ast
          (indexMap, _) = runIndexSelection ra
      pure indexMap

resultsIn :: (Show a, Eq a) => IO a -> a -> IO ()
resultsIn action expected = do
  result <- action
  result `shouldBe` expected

toSelection :: [(T.Text, [[Column]])] -> IndexMap
toSelection info = idxMap
  where
    (texts, colss) = unzip info
    f text cols = (Id text, Set.fromList $ map (Index . SearchSignature . Set.fromList) cols)
    idxMap = Map.fromList $ zipWith f texts colss

spec :: Spec
spec = fdescribe "Index selection" $ parallel $ do
  it "creates indexes for a single fact" $ do
    idxSel "single_fact" `resultsIn`
      toSelection [("another", [[0,1,2]]), ("edge", [[0,1]])]

  it "creates indexes for a single non-recursive rule" $ do
    idxSel "single_nonrecursive_rule" `resultsIn`
      toSelection [("edge", [[0,1]]), ("path", [[0,1]])]

  it "creates indexes for nested searches correctly" $ do
    idxSel "multiple_rule_clauses" `resultsIn`
      toSelection [ ("first",  [[0]])
                  , ("second", [[0,1], [1]])  -- Is only [1] possible here?
                  , ("third",  [[0,1]])
                  ]

  xit "handles multiple indexes on 1 rule correctly" $ do
    idxSel "index_selection" `resultsIn`
      toSelection [ ("a", [[0]])
                  , ("b", [[0,1]])
                  , ("c", [[0,1,2]])
                  , ("d", [[0,1]])
                  , ("triple", [[0,1,2], [2], [0,2]])
                  ]

  xit "selects a minimal set of indexes for a rule" $ do
    idxSel "minimal_index_selection" `resultsIn`
      toSelection [ ("first", [[0,1,2], [1,2], [2]])
                  , ("second", [[0,1]])
                  , ("third", [[0,1]])
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

  xit "creates indexes for mutually recursive rules" $ do
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
