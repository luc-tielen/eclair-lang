{-# LANGUAGE TemplateHaskell #-}

module Eclair.RA.IR
  ( Relation
  , RA(..)
  , RAF(..)
  , Alias
  , Clause
  , Action
  , ColumnIndex
  ) where

import Eclair.Id
import Eclair.Pretty


type Relation = Id
type Alias = Id
type Clause = RA
type Action = RA
type ColumnIndex = Int

-- NOTE: removed Insert, couldn't find a use?
data RA
  = Search Relation Alias [Clause] Action
  | Project Relation [RA]
  | Merge Relation Relation
  | Swap Relation Relation
  | Purge Relation
  | Par [RA]
  | Loop [RA]
  | Exit [Relation] -- NOTE: counttuples check is 'builtin' atm
                    -- Later this needs to be changed to Clause to deal with 'X<100' etc as well.
  | Module [RA]
  | Lit Word32
  | ColumnIndex Relation ColumnIndex
  | Constrain RA RA  -- equality constraint
  | NotElem Relation [RA]
  | If RA RA RA  -- NOTE: not the traditional if: args are lhs, rhs, body
  deriving (Eq, Show)

makeBaseFunctor ''RA


prettyBlock :: Pretty a => [a] -> Doc ann
prettyBlock = indentBlock . vsep . map pretty

indentBlock :: Doc ann -> Doc ann
indentBlock block = nest indentation (hardline <> block)

instance Pretty RA where
  pretty = \case
    Search r alias clauses inner ->
      let clausesText =
            if null clauses
              then ""
              else "where" <+> parens (withAnds $ map pretty clauses) <> space
       in "search" <+> pretty r <+> "as" <+> pretty alias <+> clausesText <> "do" <>
            prettyBlock [inner]
    Project r terms ->
      "project" <+> prettyValues terms <+>
      "into" <+> pretty r
    Merge r1 r2 -> "merge" <+> pretty r1 <+> pretty r2
    Swap r1 r2 -> "swap" <+> pretty r1 <+> pretty r2
    Purge r -> "purge" <+> pretty r
    Par stmts -> "parallel do" <> prettyBlock stmts
    Loop stmts -> "loop do" <> prettyBlock stmts
    If lhs rhs stmt ->
      "if" <+> pretty lhs <+> "=" <+> pretty rhs <+> "do" <> prettyBlock [stmt]
    Exit rs ->
      let texts = map formatExitCondition rs
      in "exit if" <+> withAnds texts
    Module stmts ->
      vsep $ map pretty stmts
    Lit x -> pretty x
    ColumnIndex r idx -> pretty r <> brackets (pretty idx)
    Constrain lhs rhs -> pretty lhs <+> "=" <+> pretty rhs
    NotElem r terms -> prettyValues terms <+> "∉" <+> pretty r
    where
      prettyValues terms = parens (withCommas $ map pretty terms)
      formatExitCondition r =
        "counttuples" <> parens (pretty r) <+> "=" <+> "0"
