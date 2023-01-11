module Eclair.AST.Transforms.NormalizeRules
  ( transform
  ) where

import Data.List (partition)
import Eclair.Transform
import Eclair.AST.IR

-- This transform prepares the AST for lowering to RA by:
-- 1. Shifting all constraints and negations to the end.

transform :: Transform AST AST
transform =
  pureTransform $ cata rewriteVars
  where
    rewriteVars = \case
      RuleF nodeId name values clauses -> do
        let (matching, rest) = partition isConstraintOrNegation clauses
            (constraints, negations) = partition isConstraint matching
        Rule nodeId name values $ rest <> constraints <> negations
      astf ->
        embed astf

    isConstraintOrNegation :: AST -> Bool
    isConstraintOrNegation = \case
      Constraint {} -> True
      Not {} -> True
      _ -> False

    isConstraint :: AST -> Bool
    isConstraint = \case
      Constraint {} -> True
      _ -> False
