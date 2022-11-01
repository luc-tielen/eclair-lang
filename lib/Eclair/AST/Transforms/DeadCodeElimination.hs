module Eclair.AST.Transforms.DeadCodeElimination
  ( transform
  ) where

import Eclair.Transform
import Eclair.AST.IR


transform :: Transform AST AST
transform =
  pureTransform $ cata $ \case
    RuleF nodeId name args clauses ->
      let clauses' = filter (not . isRedundantAssign) clauses
        in Rule nodeId name args clauses'
    astf ->
      embed astf
  where
    isRedundantAssign = \case
      Assign _ (Var _ v1) (Var _ v2) | v1 == v2 -> True
      _ -> False
