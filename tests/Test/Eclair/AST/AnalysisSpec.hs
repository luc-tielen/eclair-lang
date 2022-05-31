module Test.Eclair.AST.AnalysisSpec
  ( module Test.Eclair.AST.AnalysisSpec
  ) where

import Test.Hspec
import System.FilePath
import Eclair.AST.Analysis
import Eclair.Id
import Eclair


ungroundedVars :: FilePath -> [Text] -> IO ()
ungroundedVars path expectedVars = do
  let file = "tests/fixtures" </> path <.> "dl"
  Result ungroundedVars <- semanticAnalysis file
  let varNames = map (\(UngroundedVar _ v) -> v) ungroundedVars
  varNames `shouldBe` map Id expectedVars

spec :: Spec
spec = fdescribe "Semantic analysis" $ parallel $ do
  describe "detecting ungrounded variables" $ parallel $ do
    it "finds no ungrounded vars for empty file" $ do
      ungroundedVars "empty" []

    it "finds no ungrounded vars for top level fact with no vars" $ do
      ungroundedVars "single_fact" []

    it "finds no ungrounded vars for valid non-recursive rules" $ do
      ungroundedVars "single_nonrecursive_rule" []
      ungroundedVars "multiple_rule_clauses" []
      ungroundedVars "multiple_clauses_same_name" []

    it "finds no ungrounded vars for valid recursive rules" $ do
      ungroundedVars "single_recursive_rule" []
      ungroundedVars "mutually_recursive_rules" []

    it "finds no ungrounded vars for a rule where 2 vars are equal" pending

    it "marks all variables found in top level facts as ungrounded" $ do
      ungroundedVars "ungrounded_var_in_facts" ["a", "b", "c", "d"]

    it "marks all variables only found in a rule head as ungrounded" $ do
      ungroundedVars "ungrounded_var_in_rules" ["z", "a", "b"]

    it "finds no ungrounded vars for a rule with a unused var in the body" $ do
      ungroundedVars "ungrounded_var_check_in_rule_body" []
