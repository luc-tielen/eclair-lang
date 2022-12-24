module Eclair.Common.Operator
  ( ArithmeticOp(..)
  , ConstraintOp(..)
  , isEqualityOp
  ) where

import Eclair.Common.Pretty

data ArithmeticOp
  = Plus
  | Minus
  | Multiply
  | Divide  -- NOTE: integer division
  deriving (Eq, Show)

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

instance Pretty ArithmeticOp where
  pretty = \case
    Plus -> "+"
    Minus -> "-"
    Multiply -> "*"
    Divide -> "/"

instance Pretty ConstraintOp where
  pretty = \case
    Equals         -> "="
    NotEquals      -> "!="
    LessThan       -> "<"
    LessOrEqual    -> "<="
    GreaterThan    -> ">"
    GreaterOrEqual -> ">="
