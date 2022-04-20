{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Eclair.AST.IR
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
  ) where

import Prelude hiding (Type, fold)
import Control.Lens
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import qualified Data.Text as T
import Prettyprinter
import Eclair.Id


type Number = Int

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
