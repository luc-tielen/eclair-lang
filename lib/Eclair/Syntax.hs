{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Eclair.Syntax
  ( AST(..)
  , ASTF(..)
  , _Lit
  , _Var
  , _Atom
  , _Rule
  , _Module
  , Value
  , Clause
  , Decl
  , Number
  , Type(..)
  , Id(..)
  , prependToId
  , appendToId
  , startsWithId
  , stripIdPrefixes
  , startsWithIdPrefix
  , deltaPrefix
  , newPrefix
  ) where

import Protolude hiding (Type, fold)
import Control.Lens
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import qualified Data.Text as T
import Prettyprinter


type Number = Int

newtype Id = Id { unId :: Text }
  deriving (Eq, Ord, Show)

instance Pretty Id where
  pretty = pretty . unId

appendToId :: Id -> Text -> Id
appendToId (Id x) y = Id (x <> y)

prependToId :: Text -> Id -> Id
prependToId x (Id y) = Id (x <> y)

startsWithId :: Id -> Id -> Bool
startsWithId (Id x) (Id start) =
  start `T.isPrefixOf` x

stripIdPrefixes :: Id -> Id
stripIdPrefixes (Id x) = Id $ stripPrefixes x where
  stripPrefixes t = foldl' stripPrefix t [deltaPrefix, newPrefix]
  stripPrefix acc prefix = fromMaybe acc (T.stripPrefix prefix acc)

-- TODO: make all prefixes starts with special symbol, invalid in syntax
deltaPrefix, newPrefix :: Text
deltaPrefix = "delta_"
newPrefix = "new_"

startsWithIdPrefix :: Id -> Bool
startsWithIdPrefix (Id x) =
  any (`T.isPrefixOf` x) [deltaPrefix, newPrefix]


type Value = AST
type Clause = AST
type Decl = AST

data Type
  = U32
  deriving (Eq, Ord, Show)

data AST
  = Lit Number
  | Var Id
  | Atom Id [Value]
  | Rule Id [Value] [Clause]
  | DeclareType Id [Type]
  | Module [Decl]
  deriving (Eq, Show)

makePrisms ''AST
makeBaseFunctor ''AST
