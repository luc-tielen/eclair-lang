module Eclair.RA.Printer ( Pretty ) where

import Eclair.Pretty
import Eclair.RA.IR
import Eclair.AST.IR (Id(..))
import Protolude
import qualified Data.Text as T

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
