module Eclair.AST.Transforms.DeadCodeElimination
  ( transform
  ) where

import Eclair.Transform
import Eclair.AST.Analysis
import Eclair.AST.IR


transform :: Container RuleWithContradiction -> Transform AST AST
transform analysis =
  pureTransform $ cata $ \case
    ModuleF nodeId decls ->
       let decls' = filter (not . isContradictingRule) decls
       in Module nodeId decls'
    RuleF nodeId name args clauses ->
      let clauses' = filter (not . isRedundantAssign) clauses
        in Rule nodeId name args clauses'
    astf ->
      embed astf
  where
    contradictingRuleIds =
      map unRuleWithContradiction analysis

    isContradictingRule = \case
      Rule ruleId _ _ _ | ruleId `elem` contradictingRuleIds -> True
      _ -> False

    isRedundantAssign = \case
      Assign _ (Var _ v1) (Var _ v2) | v1 == v2 -> True
      _ -> False
