{-# OPTIONS_GHC -Wno-orphans #-}

module Eclair.RA.Printer ( Pretty, printRA ) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Eclair.RA.IR
import Eclair.Syntax (Id(..))
import Protolude
import qualified Data.Text as T

printRA :: RA -> T.Text
printRA =
  renderStrict . layoutSmart defaultLayoutOptions . pretty

indentation :: Int
indentation = 2

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
      interleaveWith d = hsep . punctuate d
      withCommas = interleaveWith comma
      withAnds = interleaveWith (space <> "and")
      prettyValues terms = parens (withCommas $ map pretty terms)
      formatExitCondition r =
        "counttuples" <> parens (pretty r) <+> "=" <+> "0"

instance Pretty Id where
  pretty = pretty . unId
