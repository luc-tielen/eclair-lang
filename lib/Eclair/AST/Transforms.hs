module Eclair.AST.Transforms
  ( simplify
  , ReplaceStrings.StringMap
  ) where

import Eclair.AST.IR
import Eclair.AST.Analysis
import Eclair.Transform
import qualified Eclair.AST.Transforms.ConstantFolding as ConstantFolding
import qualified Eclair.AST.Transforms.RemoveAliases as RemoveAliases
import qualified Eclair.AST.Transforms.DeadCodeElimination as DCE
import qualified Eclair.AST.Transforms.ReplaceStrings as ReplaceStrings
import qualified Eclair.AST.Transforms.NormalizeRules as NormalizeRules
import Eclair.Common.Extern


-- Transforms can be grouped into 3 parts:
--
-- 1. transforms that need to run a single time, before optimizations
-- 2. main optimization pipeline (runs until fixpoint is reached)
-- 3. transforms that need to run a single time, after optimizations


simplify :: NodeId -> [Extern] -> SemanticInfo -> AST -> (AST, ReplaceStrings.StringMap)
simplify nodeId externs analysis =
  runTransform nodeId
    -- Transforms before optimizations:
      $ ConstantFolding.transform

    -- Optimizations that run until fixpoint is reached:
    >>> RemoveAliases.transform externs
    >>> ConstantFolding.transform
    >>> DCE.transform (deadCodeIds analysis)

    -- Transforms after optimizations:
    >>> NormalizeRules.transform
    >>> ReplaceStrings.transform
