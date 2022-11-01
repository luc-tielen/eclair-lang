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
  pureTransform $ cata $ \case
    VarF nodeId var ->
      replaceVar pointsTo nodeId var
    astf ->
      embed astf
  where
    replaceVar pointsTo varNodeId var =
      fromMaybe (Var varNodeId var) $ Map.lookup varNodeId pointsTo
