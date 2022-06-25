module Test.Eclair.AST.AnalysisSpec
  ( module Test.Eclair.AST.AnalysisSpec
  ) where

import Test.Hspec
import System.FilePath
import Control.Exception
import Eclair.AST.Analysis
import Eclair.Id
import Eclair


check :: (Eq a, Show a) => (SemanticErrors -> [a]) -> FilePath -> [a] -> IO ()
check f path expected = do
  let file = "tests/fixtures" </> path <.> "dl"
  result <- try $ semanticAnalysis file
  case result of
    Left (SemanticErr _ _ errs) ->
      f errs `shouldBe` expected
    Left e ->
      panic $ "Received unexpected exception: " <> show e
    Right _ ->
      unless (null expected) $
        panic "Expected SA errors, but found none!"

checkUngroundedVars :: FilePath -> [Text] -> IO ()
checkUngroundedVars path expectedVars =
  check getUngroundedVars path (map Id expectedVars)
  where
    getUngroundedVars =
      map (\(UngroundedVar _ v) -> v) . ungroundedVars

checkMissingTypedefs :: FilePath -> [Text] -> IO ()
checkMissingTypedefs path expectedVars =
  check getMissingTypedefs path (map Id expectedVars)
  where
    getMissingTypedefs =
      map (\(MissingTypedef _ v) -> v) . missingTypedefs

checkEmptyModules :: FilePath -> [EmptyModule] -> IO ()
checkEmptyModules =
  check emptyModules

checkWildcardsInFacts :: FilePath -> [WildcardInFact] -> IO ()
checkWildcardsInFacts =
  check wildcardsInFacts

checkWildcardsInRuleHeads :: FilePath -> [WildcardInRuleHead] -> IO ()
checkWildcardsInRuleHeads =
  check wildcardsInRuleHeads

checkWildcardsInAssignments :: FilePath -> [WildcardInAssignment] -> IO ()
checkWildcardsInAssignments =
  check wildcardsInAssignments

checkRuleClauseSameVar :: FilePath -> [Text] -> IO ()
checkRuleClauseSameVar path expectedVars =
  check getRuleClausesSameVar path (map Id expectedVars)
  where
    getRuleClausesSameVar =
      map (\(RuleClauseSameVar _ v) -> v) . ruleClausesWithSameVar

spec :: Spec
spec = describe "Semantic analysis" $ parallel $ do
  describe "detecting empty modules" $ parallel $ do
    it "detects an empty module" $
      checkEmptyModules "empty" [EmptyModule $ NodeId 0]

    it "finds no issues for non-empty module" $ do
      checkEmptyModules "single_recursive_rule" []
      checkEmptyModules "mutually_recursive_rules" []

  describe "detecting missing type definitions" $ do
    it "detects no missing type definitions for empty file" $
      checkMissingTypedefs "empty" []

    it "detects missing type definitions for rules" $
      checkMissingTypedefs "missing_typedef_in_rule"
        ["unknown_rule", "unknown_fact1", "unknown_fact2"]

    it "detects missing type definitions for top level facts" $
      checkMissingTypedefs "missing_typedef_in_atom" ["unknown_fact"]

    it "finds no issues if all types are defined" $ do
      checkMissingTypedefs "mutually_recursive_rules" []
      checkMissingTypedefs "typedef_after_usage" []

  describe "detecting ungrounded variables" $ do
    it "finds no ungrounded vars for empty file" $ do
      checkUngroundedVars "empty" []

    it "finds no ungrounded vars for top level fact with no vars" $ do
      checkUngroundedVars "single_fact" []

    it "finds no ungrounded vars for valid non-recursive rules" $ do
      checkUngroundedVars "single_nonrecursive_rule" []
      checkUngroundedVars "multiple_rule_clauses" []
      checkUngroundedVars "multiple_clauses_same_name" []

    it "finds no ungrounded vars for valid recursive rules" $ do
      checkUngroundedVars "single_recursive_rule" []
      checkUngroundedVars "mutually_recursive_rules" []

    it "finds no ungrounded vars for a rule where 2 vars are equal" pending

    it "marks all variables found in top level facts as ungrounded" $ do
      checkUngroundedVars "ungrounded_var_in_facts" ["a", "b", "c", "d"]

    it "marks all variables only found in a rule head as ungrounded" $ do
      checkUngroundedVars "ungrounded_var_in_rules" ["z", "a", "b"]

    it "finds no ungrounded vars for a rule with a unused var in the body" $ do
      checkUngroundedVars "ungrounded_var_check_in_rule_body" []

  describe "Invalid wildcard usage" $ do
    it "reports wildcards used in a top level fact" $ do
      checkWildcardsInFacts "wildcard_in_fact"
        [ WildcardInFact (NodeId 2) (NodeId 4) 1
        , WildcardInFact (NodeId 5) (NodeId 6) 0
        ]

    it "reports wildcards used in a rule head" $ do
      checkWildcardsInRuleHeads "wildcard_in_rule_head"
        [ WildcardInRuleHead (NodeId 3) (NodeId 5) 1
        , WildcardInRuleHead (NodeId 8) (NodeId 9) 0
        ]

    it "does not report wildcards used in rule body" $ do
      checkWildcardsInFacts "wildcard_in_rule_body" []
      checkWildcardsInRuleHeads "wildcard_in_rule_body" []

    it "does not report normal variables" $ do
      checkWildcardsInFacts "single_recursive_rule" []
      checkWildcardsInRuleHeads "single_recursive_rule" []
      checkWildcardsInAssignments "single_recursive_rule" []

    it "reports wildcards used in assignments" $ do
      checkWildcardsInAssignments "wildcard_in_assignment"
        [ WildcardInAssignment (NodeId 8) (NodeId 9)
        , WildcardInAssignment (NodeId 16) (NodeId 18)
        , WildcardInAssignment (NodeId 24) (NodeId 25)
        , WildcardInAssignment (NodeId 24) (NodeId 26)
        ]

  -- NOTE: tests should be removed once feature is implemented
  describe "disabling same variables in rule clause" $ do
    it "detects rule clauses where the same variable occurs more than one time" $ do
      checkRuleClauseSameVar "rule_equal_columns" ["x", "y", "z"]

    it "finds no issues if all variables are different in a rule clause" $ do
      checkRuleClauseSameVar "single_recursive_rule" []
