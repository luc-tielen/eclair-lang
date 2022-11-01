module Eclair.AST.Transforms
  ( simplify
  , ReplaceStrings.StringMap
  ) where

import Eclair.AST.IR
import Eclair.AST.Analysis
import Eclair.Transform
import qualified Eclair.AST.Transforms.RemoveWildcards as RmWildcards
import qualified Eclair.AST.Transforms.ShiftAssignments as ShiftAssigns
import qualified Eclair.AST.Transforms.CopyPropagation as CopyPropagation
import qualified Eclair.AST.Transforms.UniqueVars as UniqueVars
import qualified Eclair.AST.Transforms.ReplaceStrings as ReplaceStrings


-- Transforms can be grouped into 3 parts:
--
-- 1. transforms that need to run a single time, before optimizations
-- 2. main optimization pipeline (runs until fixpoint is reached)
-- 3. transforms that need to run a single time, after optimizations


simplify :: NodeId -> PointsToAnalysis -> AST -> (AST, ReplaceStrings.StringMap)
simplify nodeId pointsTo = runTransform nodeId
  -- Transforms before optimizations:
  $   RmWildcards.transform
  -- Optimizations:
  >>> CopyPropagation.transform pointsTo

  -- Transforms after optimizations:
  >>> UniqueVars.transform
  >>> ShiftAssigns.transform
  >>> ReplaceStrings.transform
