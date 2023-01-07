module Eclair.Common.Extern
  ( Extern(..)
  , ExternKind(..)
  ) where

import Eclair.Common.Id


data Extern = Extern Id Int ExternKind
  deriving (Eq, Show)

data ExternKind
  = ExternConstraint
  | ExternFunction
  deriving (Eq, Show)

