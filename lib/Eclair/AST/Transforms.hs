module Eclair.AST.Transforms
  ( simplify
  ) where

import Eclair.AST.IR
import Eclair.Transform
import qualified Eclair.AST.Transforms.RemoveWildcards as RmWildcards
import qualified Eclair.AST.Transforms.ShiftAssignments as ShiftAssigns
import qualified Eclair.AST.Transforms.UniqueVars as UniqueVars


simplify :: NodeId -> AST -> AST
simplify nodeId =
  runTransform nodeId $ RmWildcards.transform
                    >>> UniqueVars.transform
                    >>> ShiftAssigns.transform
