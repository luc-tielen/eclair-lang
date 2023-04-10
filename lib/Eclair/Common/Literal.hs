module Eclair.Common.Literal
  ( Literal(..)
  ) where
import Prettyprinter (Pretty (pretty), dquotes)

data Literal
  = LNumber Word32
  | LString Text
  deriving (Eq, Show)

instance Pretty Literal where
  pretty = \case
    LNumber x -> pretty x
    LString x -> dquotes $ pretty x
