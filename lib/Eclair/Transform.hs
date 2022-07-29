module Eclair.Transform
  ( Transform(..)
  , runTransform
  , fixTransform
  , freshNodeId
  , RewriteRule
  , RewriteRuleT
  ) where

import Data.Functor.Foldable (Base)
import Control.Arrow
import Eclair.AST.IR (NodeId(..))


newtype TransformM a
  = TransformM (State NodeId a)
  deriving (Functor, Applicative, Monad) via State NodeId

freshNodeId :: TransformM NodeId
freshNodeId = TransformM $ do
  node <- get
  modify $ \(NodeId y) -> NodeId (y + 1)
  pure node

newtype Transform ir1 ir2
  = Transform (ir1 -> TransformM ir2)
  deriving (Semigroup, Monoid) via Ap (Kleisli TransformM ir1) ir2
  deriving (Category, Arrow) via Kleisli TransformM

runTransform :: NodeId -> Transform ir1 ir2 -> ir1 -> ir2
runTransform nodeId (Transform f) ir1 =
  runTransformM (f ir1)
  where
    runTransformM :: TransformM a -> a
    runTransformM (TransformM m) =
      evalState m nodeId

fixTransform :: Eq ir => Transform ir ir -> Transform ir ir
fixTransform (Transform f) =
  Transform $ fix $ \recur ir -> do
    ir' <- f ir
    if ir' == ir
      then pure ir'
      else recur ir'

type RewriteRule ir = Base ir (TransformM ir) -> TransformM ir

type RewriteRuleT t ir = Base ir (t TransformM ir) -> t TransformM ir

