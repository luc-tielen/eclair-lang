module Eclair.Souffle.IR
  ( Souffle(..)
  , ConversionError(..)
  , toSouffle
  ) where

import qualified Eclair.AST.IR as AST
import Prettyprinter
import Eclair.Common.Pretty
import Eclair.Common.Literal
import Eclair.Common.Id
import Eclair.Common.Location (NodeId)
import Eclair.Common.Operator

type AST = AST.AST

data Type
  = Unsigned
  | Symbol

data UsageMode
  = Input
  | Output

data Souffle
  = Lit Literal
  | Var Id
  | BinOp ArithmeticOp Souffle Souffle
  | Constraint LogicalOp Souffle Souffle
  | Rule Id [Souffle] [Souffle]
  | Not Souffle
  | Atom Id [Souffle]
  | DeclareType Id [Type]
  | DeclareUsage Id UsageMode
  | Module [Souffle]

data ConversionError loc
  = HoleNotSupported loc
  | UnsupportedType loc AST.Type
  | UnsupportedCase loc
  deriving Functor

toSouffle :: AST -> Either (ConversionError NodeId) Souffle
toSouffle = map Module . cata f
  where
    f :: AST.ASTF (Either (ConversionError NodeId) [Souffle])
      -> Either (ConversionError NodeId) [Souffle]
    f = \case
      AST.ModuleF _ decls -> do
        map mconcat $ sequence decls
      AST.DeclareTypeF nodeId name tys usageMode -> do
        souffleTys <- traverse (toSouffleType nodeId) tys
        let declType = DeclareType name souffleTys
            usageDecls = case usageMode of
              AST.Input ->
                one $ DeclareUsage name Input
              AST.Output ->
                one $ DeclareUsage name Output
              AST.InputOutput ->
                [ DeclareUsage name Input
                , DeclareUsage name Output
                ]
              AST.Internal ->
                mempty
        pure $ declType : usageDecls
      AST.AtomF _ name args -> do
        args' <- mconcat <$> sequence args
        pure $ one $ Atom name args'
      AST.RuleF _ name args clauses -> do
        args' <- mconcat <$> sequence args
        clauses' <- mconcat <$> sequence clauses
        pure $ one $ Rule name args' clauses'
      AST.VarF _ name ->
        pure $ one $ Var name
      AST.LitF _ lit ->
        pure $ one $ Lit lit
      AST.HoleF nodeId ->
        throwError $ HoleNotSupported nodeId
      AST.BinOpF _ op lhs rhs -> do
        lhs' <- lhs
        rhs' <- rhs
        pure $ BinOp op <$> lhs' <*> rhs'
      AST.ConstraintF _ op lhs rhs -> do
        lhs' <- lhs
        rhs' <- rhs
        pure $ Constraint op <$> lhs' <*> rhs'
      AST.NotF _ inner -> do
        inner' <- inner
        pure $ Not <$> inner'
      astf ->
        let nodeId = AST.getNodeIdF astf
        in throwError $ UnsupportedCase nodeId

    toSouffleType :: NodeId -> AST.Type -> Either (ConversionError NodeId) Type
    toSouffleType nodeId = \case
      AST.U32 -> pure Unsigned
      AST.Str -> pure Symbol
      ty -> throwError $ UnsupportedType nodeId ty


instance Pretty Type where
  pretty = \case
    Unsigned -> "unsigned"
    Symbol -> "symbol"

data RenderPosition = TopLevel | Nested

instance Pretty Souffle where
  pretty souffleIR = runReader (pretty' souffleIR) TopLevel
    where
      pretty' = \case
        Lit x ->
          pure $ pretty x
        Var v ->
          pure $ pretty v
        BinOp op lhs rhs -> do
          lhs' <- pretty' lhs
          rhs' <- pretty' rhs
          pure $ parens $ lhs' <+> pretty op <+> rhs'
        Constraint op lhs rhs -> do
          lhs' <- pretty' lhs
          rhs' <- pretty' rhs
          pure $ lhs' <+> pretty op <+> rhs'
        Not clause ->
          ("!" <>) <$> pretty' clause
        Atom name values -> do
          end <- ask <&> \case
            TopLevel -> "."
            Nested -> mempty
          values' <- traverse pretty' values
          pure $ pretty name <> parens (withCommas values') <> end
        Rule name values clauses -> do
          (values', clauses') <- local (const Nested) $ do
            (,) <$> traverse pretty' values <*> traverse pretty' clauses
          let separators = replicate (length clauses - 1) "," <> ["."]
          pure $ pretty name <> parens (withCommas values') <+> ":-" <> hardline <>
                indent 2 (vsep (zipWith (<>) clauses' separators))
        DeclareType name tys -> do
          let argNames = map (\col -> "arg_" <> show col) [0 :: Int ..]
              args = zipWith (\argName ty -> argName <> ":" <+> pretty ty) argNames tys
          pure $ ".decl" <+> pretty name <> parens (withCommas args)
        DeclareUsage name usageMode ->
          pure $ case usageMode of
            Input -> ".input" <+> pretty name
            Output -> ".output" <+> pretty name
        Module decls -> do
          decls' <- traverse pretty' decls
          pure $ vsep $ intersperse mempty decls'
