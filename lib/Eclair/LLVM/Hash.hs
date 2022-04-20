{-# LANGUAGE TypeApplications, UndecidableInstances, TypeOperators, DefaultSignatures #-}

module Eclair.LLVM.Hash
  ( Hash
  , unHash
  , HashEnum(..)
  , HashWithPrefix(..)
  , HashOnly(..)
  , ToHash(..)
  ) where

import qualified Data.Text as T


newtype Hash = Hash { unHash :: T.Text }

instance Semigroup Hash where
  Hash h1 <> Hash h2 =
    Hash $ h1 <> "__" <> h2

newtype HashEnum a = HashEnum a

newtype HashWithPrefix (prefix :: Symbol) a
  = HashWithPrefix a

newtype HashOnly (ty :: Symbol) a = HashOnly a

class ToHash a where
  getHash :: a -> Hash
  default getHash :: (Generic a, GToHash (Rep a)) => a -> Hash
  getHash a = gGetHash (from a)

instance ToHash T.Text where
  getHash = Hash

instance ToHash Int where
  getHash = Hash . T.pack . show

instance ToHash Word64 where
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
  getHash (HashWithPrefix a) =
    let pre = Hash $ T.pack $ symbolVal (Proxy :: Proxy prefix)
        h = gGetHash (from a)
     in pre <> h

instance (HasField ty a b, ToHash b) => ToHash (HashOnly ty a) where
  getHash (HashOnly a) =
    getHash $ getField @ty a

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

