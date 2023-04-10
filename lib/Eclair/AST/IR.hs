{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Eclair.AST.IR
  ( AST(.., PWildcard)
  , ASTF(.., PWildcardF)
  , Value
  , Clause
  , Decl
  , Literal(..)
  , Type(..)
  , ArithmeticOp(..)
  , LogicalOp(..)
  , isEqualityOp
  , getNodeId
  , getNodeIdF
  , getExternDefs
  , UsageMode(..)
  , Attributes
  ) where

import Prettyprinter
import Eclair.Common.Id
import Eclair.Common.Operator
import Eclair.Common.Extern
import Eclair.Common.Literal
import Eclair.Common.Pretty
import Eclair.Common.Location

type Value = AST
type Clause = AST
type Decl = AST

data Type
  = U32
  | Str
  | TUnknown Int  -- NOTE: unification variable, only used internally!
  deriving (Eq, Ord, Show)

data UsageMode
  = Input
  | Output
  | InputOutput
  | Internal  -- This variant is only used internally (pun intended).
  deriving (Eq, Show)

-- Later this will also contain (Maybe StorageType), ...
type Attributes = UsageMode

-- NOTE: There is no explicit "AND" node, conjunctions are inlined into other
-- nodes (as lists of clauses).
data AST
  -- Expressions
  = Lit NodeId Literal
  | Var NodeId Id
  | Hole NodeId
  | BinOp NodeId ArithmeticOp AST AST
  -- Statements
  | Constraint NodeId LogicalOp AST AST
  | Rule NodeId Id [Value] [Clause]
  | Not NodeId Clause
  | Atom NodeId Id [Value]  -- Can be both a Datalog relation, or a externally defined function / constraint
  | ExternDefinition NodeId Id [Type] (Maybe Type)
  | DeclareType NodeId Id [Type] Attributes
  | Module NodeId [Decl]
  deriving (Eq, Show)

pattern PWildcard :: NodeId -> AST
pattern PWildcard nodeId
  = Var nodeId (Id "_")

makeBaseFunctor ''AST

pattern PWildcardF :: NodeId -> ASTF r
pattern PWildcardF nodeId
  = VarF nodeId (Id "_")

getNodeId :: AST -> NodeId
getNodeId = \case
  Module nodeId _ -> nodeId
  DeclareType nodeId _ _ _ -> nodeId
  ExternDefinition nodeId _ _ _ -> nodeId
  Rule nodeId _ _ _ -> nodeId
  Not nodeId _ -> nodeId
  Atom nodeId _ _ -> nodeId
  BinOp nodeId _ _ _ -> nodeId
  Constraint nodeId _ _ _ -> nodeId
  Lit nodeId _ -> nodeId
  Var nodeId _ -> nodeId
  Hole nodeId -> nodeId

getNodeIdF :: ASTF a -> NodeId
getNodeIdF = \case
  ModuleF nodeId _ -> nodeId
  DeclareTypeF nodeId _ _ _ -> nodeId
  ExternDefinitionF nodeId _ _ _ -> nodeId
  RuleF nodeId _ _ _ -> nodeId
  NotF nodeId _ -> nodeId
  AtomF nodeId _ _ -> nodeId
  BinOpF nodeId _ _ _ -> nodeId
  ConstraintF nodeId _ _ _ -> nodeId
  LitF nodeId _ -> nodeId
  VarF nodeId _ -> nodeId
  HoleF nodeId -> nodeId

getExternDefs :: AST -> [Extern]
getExternDefs = cata $ \case
  ExternDefinitionF _ name argTys mRetTy ->
    let extKind = if isJust mRetTy then ExternFunction else ExternConstraint
     in one $ Extern name (length argTys) extKind
  astf ->
    fold astf

instance Pretty Type where
  pretty = \case
    U32 -> "u32"
    Str -> "string"
    TUnknown x -> "ty" <> show x

data RenderPosition = TopLevel | Nested

instance Pretty AST where
  pretty ast = runReader (pretty' ast) TopLevel
    where
      pretty' = \case
        Lit _ x ->
          pure $ pretty x
        Var _ v ->
          pure $ pretty v
        Hole _ ->
          pure "?"
        BinOp _ op lhs rhs -> do
          lhs' <- pretty' lhs
          rhs' <- pretty' rhs
          pure $ parens $ lhs' <+> pretty op <+> rhs'
        Constraint _ op lhs rhs -> do
          lhs' <- pretty' lhs
          rhs' <- pretty' rhs
          pure $ lhs' <+> pretty op <+> rhs'
        Not _ clause ->
          ("!" <>) <$> pretty' clause
        Atom _ name values -> do
          end <- ask <&> \case
            TopLevel -> "."
            Nested -> mempty
          values' <- traverse pretty' values
          pure $ pretty name <> parens (withCommas values') <> end
        Rule _ name values clauses -> do
          (values', clauses') <- local (const Nested) $ do
            (,) <$> traverse pretty' values <*> traverse pretty' clauses
          let separators = replicate (length clauses - 1) "," ++ ["."]
          pure $ pretty name <> parens (withCommas values') <+> ":-" <> hardline <>
                indent 2 (vsep (zipWith (<>) clauses' separators))
        ExternDefinition _ name argTys mRetTy -> do
          let prettyRetTy = case mRetTy of
                Just retTy -> " " <> pretty retTy
                Nothing    -> mempty
          pure $ "@extern" <+> pretty name <> parens (withCommas $ map pretty argTys)
                    <> prettyRetTy <> "."
        DeclareType _ name tys attrs ->
          pure $ "@def"
            <+> pretty name
             <> parens (withCommas $ map pretty tys)
             <> prettyAttrs
             <> "."
          where
            prettyAttrs = case attrs of
              Internal -> ""
              Input -> " input"
              Output -> " output"
              InputOutput -> " input output"
        Module _ decls -> do
          decls' <- traverse pretty' decls
          pure $ vsep $ intersperse mempty decls'
