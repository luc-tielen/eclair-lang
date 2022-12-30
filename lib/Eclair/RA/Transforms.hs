module Eclair.RA.Transforms
  ( simplify
  ) where

import Eclair.Common.Location (NodeId(..))
import Eclair.RA.IR
import Eclair.Transform
import qualified Eclair.RA.Transforms.HoistConstraints as HoistConstraints

simplify :: RA -> RA
simplify = runTransform (NodeId 0)
  HoistConstraints.transform
