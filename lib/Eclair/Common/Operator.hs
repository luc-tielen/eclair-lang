module Eclair.Common.Operator
  ( ArithmeticOp(..)
  , LogicalOp(..)
  , isEqualityOp
  , invertLogicalOp
  ) where

import Eclair.Common.Pretty

data ArithmeticOp
  = Plus
  | Minus
  | Multiply
  | Divide  -- NOTE: integer division
  deriving (Eq, Show)

data LogicalOp
  = Equals          -- =
  | NotEquals       -- !=
  | LessThan        -- <
  | LessOrEqual     -- <=
  | GreaterThan     -- >
  | GreaterOrEqual  -- >=
  deriving (Eq, Show)

invertLogicalOp :: LogicalOp -> LogicalOp
invertLogicalOp = \case
  Equals -> NotEquals
  NotEquals -> Equals
  LessThan -> GreaterOrEqual
  GreaterThan -> LessOrEqual
  LessOrEqual -> GreaterThan
  GreaterOrEqual -> LessThan

isEqualityOp :: LogicalOp -> Bool
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

instance Pretty LogicalOp where
  pretty = \case
    Equals         -> "="
    NotEquals      -> "!="
    LessThan       -> "<"
    LessOrEqual    -> "<="
    GreaterThan    -> ">"
    GreaterOrEqual -> ">="
