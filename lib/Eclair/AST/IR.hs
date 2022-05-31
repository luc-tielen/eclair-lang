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
  , NodeId(..)
  ) where

import Control.Lens
import Data.Functor.Foldable.TH
import Prettyprinter
import Eclair.Id
import qualified Language.Souffle.Marshal as S

newtype NodeId
  = NodeId
  { unNodeId :: Int32
  } deriving (Eq, Ord, Show, Generic)
  deriving S.Marshal

type Number = Word32

type Value = AST
type Clause = AST
type Decl = AST

data Type
  = U32
  deriving (Eq, Ord, Show)

data AST
  = Lit NodeId Number
  | Var NodeId Id
  | Atom NodeId Id [Value]
  | Rule NodeId Id [Value] [Clause]
  | DeclareType NodeId Id [Type]
  | Module NodeId [Decl]
  deriving (Eq, Show)

makePrisms ''AST
makeBaseFunctor ''AST
