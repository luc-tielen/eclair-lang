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
import Eclair.Pretty
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

instance Pretty Type where
  pretty = \case
    U32 -> "u32"

instance Pretty AST where
  pretty = \case
    Lit _ x ->
      pretty x
    Var _ v ->
      pretty v
    Atom _ name values ->
      pretty name <> parens (withCommas $ map pretty values)
    Rule _ name values clauses ->
      let separators = replicate (length clauses - 1) "," ++ ["."]
       in pretty name <> parens (withCommas $ map pretty values) <+> ":-" <> hardline <>
            indent 2 (vsep (zipWith (<>) (map pretty clauses) separators))
    DeclareType _ name tys ->
      "@def" <+> pretty name <> parens (withCommas $ map pretty tys)
    Module _ decls ->
      vsep $ intersperse hardline $ map pretty decls
