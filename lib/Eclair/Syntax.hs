{-# LANGUAGE TemplateHaskell #-}

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
  ) where


import Control.Lens
import Protolude


type Number = Int

newtype Id = Id Text
  deriving (Eq, Ord, Show)

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
