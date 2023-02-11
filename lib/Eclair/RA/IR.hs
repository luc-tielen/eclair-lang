{-# LANGUAGE TemplateHaskell #-}

module Eclair.RA.IR
  ( Relation
  , RA(..)
  , RAF(..)
  , Alias
  , Clause
  , Action
  , ColumnIndex
  , LogicalOp(..)
  , ArithmeticOp(..)
  , Op(..)
  ) where

import Eclair.Common.Id
import Eclair.Common.Pretty
import Eclair.Common.Operator
import Eclair.Common.Location (NodeId(..))


type Relation = Id
type Alias = Id
type Clause = RA
type Action = RA
type ColumnIndex = Int

data Op
  = BuiltinOp ArithmeticOp
  | ExternOp Id
  deriving (Eq, Show)

-- NOTE: removed Insert, couldn't find a use?
data RA
  = Search NodeId Relation Alias [Clause] Action
  | Project NodeId Relation [RA]
  | Merge NodeId Relation Relation
  | Swap NodeId Relation Relation
  | Purge NodeId Relation
  | Par NodeId [RA]
  | Loop NodeId [RA]
  -- NOTE: counttuples check is 'builtin' atm
  -- Later this needs to be changed to Clause to deal with 'X<100' etc as well.
  | Exit NodeId [Relation]
  | Module NodeId [RA]
  | Lit NodeId Word32
  | Undef NodeId
  | ColumnIndex NodeId Relation ColumnIndex
  | CompareOp NodeId LogicalOp RA RA
  | PrimOp NodeId Op [RA]
  | NotElem NodeId Relation [RA]
  | If NodeId RA RA  -- NOTE: args are condition and body
  deriving (Eq, Show)

makeBaseFunctor ''RA


prettyBlock :: Pretty a => [a] -> Doc ann
prettyBlock = indentBlock . vsep . map pretty

indentBlock :: Doc ann -> Doc ann
indentBlock block = nest indentation (hardline <> block)

instance Pretty Op where
  pretty = \case
    BuiltinOp op -> pretty op
    ExternOp opName -> pretty opName

instance Pretty RA where
  pretty = \case
    Search _ r alias clauses inner ->
      let clausesText =
            if null clauses
              then ""
              else "where" <+> parens (withAnds $ map pretty clauses) <> space
       in "search" <+> pretty r <+> "as" <+> pretty alias <+> clausesText <> "do" <>
            prettyBlock [inner]
    Project _ r terms ->
      "project" <+> prettyValues terms <+>
      "into" <+> pretty r
    Merge _ r1 r2 -> "merge" <+> pretty r1 <+> pretty r2
    Swap _ r1 r2 -> "swap" <+> pretty r1 <+> pretty r2
    Purge _ r -> "purge" <+> pretty r
    Par _ stmts -> "parallel do" <> prettyBlock stmts
    Loop _ stmts -> "loop do" <> prettyBlock stmts
    If _ cond stmt ->
      "if" <+> pretty cond <+> "do" <> prettyBlock [stmt]
    Exit _ rs ->
      let texts = map formatExitCondition rs
      in "exit if" <+> withAnds texts
    Module _ stmts ->
      vsep $ map pretty stmts
    Lit _ x -> pretty x
    Undef _ -> "undefined"
    ColumnIndex _ r idx -> pretty r <> brackets (pretty idx)
    CompareOp _ op lhs rhs -> pretty lhs <+> pretty op <+> pretty rhs
    PrimOp _ op args ->
      case (op, args) of
        (BuiltinOp{}, [lhs, rhs]) ->
          parens $ pretty lhs <+> pretty op <+> pretty rhs
        _ ->
          pretty op <> parens (withCommas $ map pretty args)
    NotElem _ r terms -> prettyValues terms <+> "∉" <+> pretty r
    where
      prettyValues terms = parens (withCommas $ map pretty terms)
      formatExitCondition r =
        "counttuples" <> parens (pretty r) <+> "=" <+> "0"
