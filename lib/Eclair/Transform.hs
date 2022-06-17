module Eclair.Transform
  ( Transform(..)
  , runTransform
  , fixTransform
  ) where

import Control.Arrow

newtype Transform ir1 ir2
  = Transform (ir1 -> ir2)
  deriving (Semigroup, Monoid) via (ir1 -> ir2)
  deriving (Category, Arrow) via (->)


runTransform :: Transform ir1 ir2 -> ir1 -> ir2
runTransform (Transform f) =
  f

fixTransform :: Eq ir => Transform ir ir -> ir -> ir
fixTransform (Transform f) =
  fix $ \recur ir ->
    let ir' = f ir
     in if ir' == ir
      then ir'
      else recur ir'
