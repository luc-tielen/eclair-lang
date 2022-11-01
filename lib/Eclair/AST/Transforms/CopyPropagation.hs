module Eclair.AST.Transforms.CopyPropagation
  ( transform
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Eclair.Transform
import Eclair.AST.Analysis (PointsToAnalysis(..))
import Eclair.AST.IR
import Eclair.Id
import Eclair.Comonads

-- This transform tries to reduce the amount of helper variables used in
-- assignments using the "copy propagation" algorithm.
-- This is done both to reduce complexity in rest of the compiler, and to optimize the generated code.

transform :: PointsToAnalysis -> Transform AST AST
transform (PointsToAnalysis pointsTo) =
  pureTransform $ zygo replaceVars rewrite
  where
    replaceVars = \case
      VarF nodeId var ->
        replaceWithBoundVar pointsTo nodeId var
      astf ->
        embed astf

    replaceWithBoundVar :: Map NodeId AST -> NodeId -> Id -> AST
    replaceWithBoundVar pointsTo varNodeId var =
      fromMaybe (Var varNodeId var) $ Map.lookup varNodeId pointsTo

    -- NOTE: first element in tuple is AST with all vars replaced with bound vars, second is original AST
    rewrite :: ASTF (AST, AST) -> AST
    rewrite = \case
      RuleF nodeId name args clauses ->
        let args' = map fst args
            clauses' = filter (not . isRedundantAssign) $ map fst clauses
         in Rule nodeId name args' clauses'
      astf ->
        embed $ map snd astf

    isRedundantAssign = \case
      Assign _ (Var _ v1) (Var _ v2) | v1 == v2 -> True
      _ -> False
