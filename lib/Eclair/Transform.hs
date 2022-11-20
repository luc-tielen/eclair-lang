module Eclair.Transform
  ( Transform(..)
  , pureTransform
  , TransformM
  , runTransform
  , fixTransform
  , freshNodeId
  , RewriteRule
  , RewriteRuleT
  ) where

import Eclair.AST.IR (NodeId(..))


-- A helper monad that provides a fresh supply of unused node IDs.
newtype TransformM a
  = TransformM (State NodeId a)
  deriving (Functor, Applicative, Monad) via State NodeId

-- | Generates a fresh 'NodeId'.
freshNodeId :: TransformM NodeId
freshNodeId = TransformM $ do
  node <- get
  modify $ \(NodeId y) -> NodeId (y + 1)
  pure node

-- | The main type in this module. A transform is an effectful function from
-- one type to another. Usually the type variables 'a' and 'b' represent IRs in
-- the compiler. (These IRs can potentially be the same, which can be useful
-- for creating "rewrite rules"). Transforms can be composed together using the
-- instances and functions defined in this module.
newtype Transform a b
  = Transform (a -> TransformM b)
  deriving (Semigroup, Monoid) via Ap (Kleisli TransformM a) b
  deriving (Category, Arrow) via Kleisli TransformM

-- | Helper function for creating a transform that doesn't require any effects.
pureTransform :: (a -> b) -> Transform a b
pureTransform f =
  Transform $ pure . f

-- | Converts a 'Transform' to an equivalent pure function.
--   The 'NodeId' that is passed in is used as a starting point for generating
--   additional IDs during the transform (if necessary).
runTransform :: NodeId -> Transform ir1 ir2 -> ir1 -> ir2
runTransform nodeId (Transform f) ir1 =
  runTransformM (f ir1)
  where
    runTransformM :: TransformM a -> a
    runTransformM (TransformM m) =
      evalState m nodeId

-- | A function that recursively keeps applying a 'Transform' until a fixpoint
--   is reached and no more changes occur.
fixTransform :: Eq ir => Transform ir ir -> Transform ir ir
fixTransform (Transform f) =
  Transform $ fix $ \recur ir -> do
    ir' <- f ir
    if ir' == ir
      then pure ir'
      else recur ir'

-- | Helper type synonym for a rewrite rule, making use of the recursion-schemes library.
--   This is the simplest form of rewrite rule, which allows no extra effects
--   except for the state managed by the 'Transform' itself.
type RewriteRule ir = Base ir (TransformM ir) -> TransformM ir

-- | Helper type synonym for a rewrite rule, making use of the recursion-schemes library.
--   This type of rewrite rule allows adding additional effects via the
--   monad transformer 't'.
type RewriteRuleT t ir = Base ir (t TransformM ir) -> t TransformM ir

