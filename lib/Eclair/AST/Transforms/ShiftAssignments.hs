module Eclair.AST.Transforms.ShiftAssignments
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
        let (assignClauses, restClauses) = partition isAssignment clauses'
            clauses'' = restClauses <> assignClauses
        pure $ Rule nodeId name values' clauses''
      astf ->
        embed <$> sequence astf

    isAssignment :: AST -> Bool
    isAssignment = \case
      Assign {} -> True
      _ -> False

