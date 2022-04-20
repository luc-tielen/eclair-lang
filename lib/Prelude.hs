module Prelude
  ( module Relude
  , module Control.Monad.Writer.Strict
  , module Control.Monad.RWS.Strict
  , module Control.Monad.Except
  , module Control.Category
  , module Control.Comonad
  , module GHC.TypeLits
  , module GHC.Generics
  , module GHC.Records
  , IsString(..)
  , map
  , panic
  , groupBy
  ) where

import Relude hiding ( Type, Constraint, swap, and, id, (.), map)
import Data.String (IsString(..))
import Control.Comonad
import Control.Category
import Control.Monad.Writer.Strict hiding (pass)
import Control.Monad.RWS.Strict hiding (pass)
import Control.Monad.Except
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHC.Generics (Rep, K1(..), U1(..), M1(..), (:*:)(..), from, to)
import GHC.Records (HasField(..))

map :: Functor f => (a -> b) -> f a -> f b
map = fmap
{-# INLINABLE map #-}

panic :: Text -> a
panic = error

groupBy :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupBy eq  = \case
  [] -> []
  (x:xs) ->  (x :| ys) : groupBy eq zs
    where (ys,zs) = span (eq x) xs

