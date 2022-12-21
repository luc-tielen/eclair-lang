module Eclair.AST.Transforms.ShiftConstraints
  ( transform
  ) where

import Data.List (partition)
import Eclair.Transform
import Eclair.AST.IR


transform :: Transform AST AST
transform =
  Transform $ cata rewrite
  where
    rewrite :: RewriteRule AST
    rewrite = \case
      RuleF nodeId name values clauses -> do
        values' <- sequence values
        clauses' <- sequence clauses
        let (constraintClauses, restClauses) = partition isConstraint clauses'
            clauses'' = restClauses <> constraintClauses
        pure $ Rule nodeId name values' clauses''
      astf ->
        embed <$> sequence astf

    isConstraint :: AST -> Bool
    isConstraint = \case
      Constraint {} -> True
      _ -> False

