module Eclair.AST.Transforms.DeadCodeElimination
  ( transform
  ) where

import Eclair.Transform
import Eclair.AST.Analysis
import Eclair.AST.IR


transform :: Container DeadCode -> Transform AST AST
transform analysis =
  pureTransform $ cata $ \case
    ModuleF nodeId decls ->
      Module nodeId $ filter (not . isDead) decls
    RuleF nodeId name args clauses ->
      Rule nodeId name args $
        filter (\c -> not (isDead c || isRedundantAssign c)) clauses
    astf ->
      embed astf
  where
    deadCodeNodeIds =
      map unDeadCode analysis

    isDead ast =
      getNodeId ast `elem` deadCodeNodeIds

    isRedundantAssign = \case
      Constraint _ Equals lhs rhs -> lhs == rhs
      _ -> False
