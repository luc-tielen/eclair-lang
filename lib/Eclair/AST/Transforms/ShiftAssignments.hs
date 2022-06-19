module Eclair.AST.Transforms.ShiftAssignments
  ( transform
  ) where

import Data.Functor.Foldable hiding (fold)
import Data.List (partition)
import Eclair.Transform
import Eclair.AST.IR
import Eclair.Id


transform :: Transform AST AST
transform =
  Transform $ cata rewrite
  where
    rewrite = \case
      RuleF nodeId name values clauses ->
        let (assignClauses, restClauses) = partition isAssignment clauses
            clauses' = restClauses <> assignClauses
         in Rule nodeId name values clauses'
      astf ->
        embed astf

    isAssignment :: AST -> Bool
    isAssignment = \case
      Assign {} -> True
      _ -> False
