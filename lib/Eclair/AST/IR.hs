{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Eclair.AST.IR
  ( AST(.., PWildcard)
  , ASTF(.., PWildcardF)
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
  | Assign NodeId AST AST
  | Atom NodeId Id [Value]
  | Rule NodeId Id [Value] [Clause]
  | DeclareType NodeId Id [Type]
  | Module NodeId [Decl]
  deriving (Eq, Show)

pattern PWildcard nodeId
  = Var nodeId (Id "_")

makePrisms ''AST
makeBaseFunctor ''AST

pattern PWildcardF nodeId
  = VarF nodeId (Id "_")

instance Pretty Type where
  pretty = \case
    U32 -> "u32"

data RenderPosition = TopLevel | Nested

instance Pretty AST where
  pretty ast = runReader (pretty' ast) TopLevel
    where
      pretty' = \case
        Lit _ x ->
          pure $ pretty x
        Var _ v ->
          pure $ pretty v
        Assign _ lhs rhs ->
          pure $ pretty lhs <+> "=" <+> pretty rhs
        Atom _ name values -> do
          end <- ask <&> \case
            TopLevel -> "."
            Nested -> mempty
          values' <- traverse pretty' values
          pure $ pretty name <> parens (withCommas values') <> end
        Rule _ name values clauses -> do
          let separators = replicate (length clauses - 1) "," ++ ["."]
          clauses' <- local (const Nested) $ traverse pretty' clauses
          pure $ pretty name <> parens (withCommas $ map pretty values) <+> ":-" <> hardline <>
                indent 2 (vsep (zipWith (<>) clauses' separators))
        DeclareType _ name tys ->
          pure $ "@def" <+> pretty name <> parens (withCommas $ map pretty tys) <> "."
        Module _ decls -> do
          decls' <- traverse pretty' decls
          pure $ vsep $ intersperse mempty decls'
