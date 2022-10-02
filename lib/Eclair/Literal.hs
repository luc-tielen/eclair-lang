module Eclair.Literal
  ( Literal(..)
  ) where

data Literal
  = LNumber Word32
  | LString Text
  deriving (Eq, Show)

