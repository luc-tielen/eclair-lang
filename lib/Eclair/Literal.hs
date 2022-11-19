module Eclair.Literal
  ( Literal(..)
  ) where
import Prettyprinter (Pretty (pretty))

data Literal
  = LNumber Word32
  | LString Text
  deriving (Eq, Show)

instance Pretty Literal where
  pretty = \case
    LNumber x -> pretty x
    LString x -> pretty x


