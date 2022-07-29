module Eclair.Transform
  ( Transform(..)
  , runTransform
  , fixTransform
  , RewriteRule
  , RewriteRuleT
  ) where

import Control.Arrow
import Eclair.AST.IR  -- TODO: split nodeid into separate module?
import Data.Functor.Foldable (Base)


-- TODO: rename to Supply NodeId
-- TODO freshNodeId function
newtype TransformM a
  = TransformM (State NodeId a)
  deriving (Functor, Applicative, Monad) via State NodeId

runTransformM :: NodeId -> TransformM a -> a
runTransformM nodeId (TransformM m) =
  evalState m nodeId

newtype Transform ir1 ir2
  = Transform (ir1 -> TransformM ir2)
  deriving (Semigroup, Monoid) via Ap (Kleisli TransformM ir1) ir2
  deriving (Category, Arrow) via Kleisli TransformM

type RewriteRule ir = Base ir (TransformM ir) -> TransformM ir

type RewriteRuleT t ir = Base ir (t TransformM ir) -> t TransformM ir


runTransform :: NodeId -> Transform ir1 ir2 -> ir1 -> ir2
runTransform nodeId (Transform f) ir1 =
  runTransformM nodeId (f ir1)

fixTransform :: Eq ir => Transform ir ir -> Transform ir ir
fixTransform (Transform f) =
  Transform $ fix $ \recur ir -> do
    ir' <- f ir
    if ir' == ir
      then pure ir'
      else recur ir'
