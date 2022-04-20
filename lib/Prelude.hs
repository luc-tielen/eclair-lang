module Prelude
  ( module Protolude
  , module Control.Monad.Writer.Strict
  , module Control.Monad.RWS.Strict
  , module Control.Category
  , module Control.Comonad
  , String
  , IsString(..)
  , (&&&)
  ) where

import Protolude hiding ( Meta, Type, TypeError, typeOf, Constraint, swap, bit, and, fold, pass, (.), handle, (<.>) )
import Data.String (String, IsString(..))
import Control.Comonad
import Control.Category
import Control.Arrow ((&&&))
import Control.Monad.Writer.Strict
import Control.Monad.RWS.Strict
