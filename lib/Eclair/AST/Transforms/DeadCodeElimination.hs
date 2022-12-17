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
      Assign _ (Var _ v1) (Var _ v2) | v1 == v2 -> True
      _ -> False
