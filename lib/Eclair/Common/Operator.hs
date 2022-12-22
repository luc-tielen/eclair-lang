module Eclair.Common.Operator
  ( ConstraintOp(..)
  , isEqualityOp
  ) where

import Eclair.Common.Pretty

data ConstraintOp
  = Equals          -- =
  | NotEquals       -- !=
  | LessThan        -- <
  | LessOrEqual     -- <=
  | GreaterThan     -- >
  | GreaterOrEqual  -- >=
  deriving (Eq, Show)

isEqualityOp :: ConstraintOp -> Bool
isEqualityOp = \case
  Equals -> True
  NotEquals -> True
  _ -> False

instance Pretty ConstraintOp where
  pretty = \case
    Equals         -> "="
    NotEquals      -> "!="
    LessThan       -> "<"
    LessOrEqual    -> "<="
    GreaterThan    -> ">"
    GreaterOrEqual -> ">="
