{-# LANGUAGE UndecidableInstances, TypeOperators, DefaultSignatures #-}

module Eclair.Runtime.Hash
  ( Hash
  , unHash
  , HashEnum(..)
  , HashWithPrefix(..)
  , ToHash(..)
  ) where

import Protolude
import qualified Data.Text as T


newtype Hash = Hash { unHash :: T.Text }

instance Semigroup Hash where
  Hash h1 <> Hash h2 =
    Hash $ h1 <> "__" <> h2

newtype HashEnum a = HashEnum a

newtype HashWithPrefix (prefix :: Symbol) a
  = HashWithPrefix a


class ToHash a where
  getHash :: a -> Hash
  default getHash :: (Generic a, GToHash (Rep a)) => a -> Hash
  getHash a = gGetHash (from a)

instance ToHash T.Text where
  getHash = Hash

instance ToHash Int where
  getHash = Hash . T.pack . show

instance ToHash a => ToHash [a] where
  getHash = getFoldableHash

instance ToHash a => ToHash (NonEmpty a) where
  getHash = getFoldableHash

instance ToHash a => ToHash (Set a) where
  getHash = getFoldableHash

getFoldableHash :: (Foldable f, ToHash a) => f a -> Hash
getFoldableHash =
  Hash . mconcat . intersperse "_" . map (unHash . getHash) . toList

instance (Enum a) => ToHash (HashEnum a) where
  getHash (HashEnum a) = getHash $ fromEnum a

instance forall prefix a. (KnownSymbol prefix, Generic a, GToHash (Rep a))
  => ToHash (HashWithPrefix prefix a) where
  getHash x@(HashWithPrefix a) =
    let pre = Hash $ T.pack $ symbolVal (Proxy :: Proxy prefix)
        h = gGetHash (from a)
     in pre <> h


class GToHash f where
  gGetHash :: f a -> Hash

instance ToHash a => GToHash (K1 i a) where
  gGetHash (K1 x) = getHash x

instance GToHash U1 where
  gGetHash U1 = Hash "0"

instance GToHash a => GToHash (M1 i c a) where
  gGetHash (M1 x) = gGetHash x

instance (GToHash f, GToHash g) => GToHash (f :*: g) where
  gGetHash (a :*: b) =
    gGetHash a <> gGetHash b

