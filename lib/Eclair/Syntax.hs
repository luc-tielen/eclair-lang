{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Eclair.Syntax
  ( AST(..)
  , _Lit
  , _Var
  , _Atom
  , _Rule
  , _Module
  , Value
  , Clause
  , Decl
  , Number
  , Id(..)
  , prependToId
  , appendToId
  , startsWithId
  , stripIdPrefixes
  , deltaPrefix
  , newPrefix
  , scc
  ) where

import Control.Lens
import Protolude
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Text as T
import Protolude.Unsafe (unsafeFromJust)


type Number = Int

newtype Id = Id { unId :: Text }
  deriving (Eq, Ord, Show)

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

type Value = AST
type Clause = AST
type Decl = AST

data AST
  = Lit Number
  | Var Id
  | Atom Id [Value]
  | Rule Id [Value] [Clause]
  | Module [Decl]
  deriving (Eq, Show)

makePrisms ''AST


scc :: AST -> [[AST]]
scc = \case
  Module decls -> map G.flattenSCC sortedDecls where
    -- TODO: fix issue when loose atom does not appear
    sortedDecls = G.stronglyConnComp $ zipWith (\i d -> (d, i, refersTo d)) [0..] decls
    declLineMapping = M.fromListWith (++) $ zipWith (\i d -> (nameFor d, [i])) [0 :: Int ..] decls
    refersTo = \case
      Rule _ _ clauses -> concatMap (unsafeFromJust . flip M.lookup declLineMapping . nameFor) clauses
      _ -> []
    -- TODO use traversals?
    nameFor = \case
      Atom name _ -> name
      Rule name _ _ -> name
      _ -> Id ""  -- TODO how to handle?
  _ -> panic "Unreachable code in 'scc'"

