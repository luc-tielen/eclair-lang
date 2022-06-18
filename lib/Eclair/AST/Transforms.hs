module Eclair.AST.Transforms
  ( simplify
  ) where

import Eclair.AST.IR
import Eclair.Transform
import qualified Eclair.AST.Transforms.RemoveWildcards as RmWildcards
import qualified Eclair.AST.Transforms.ShiftAssignments as ShiftAssigns


simplify :: AST -> AST
simplify =
  runTransform $ RmWildcards.transform
             >>> ShiftAssigns.transform
