{-# LANGUAGE TemplateHaskell #-}

module Eclair.EIR.IR
  ( EIR(..)
  , EIRF(..)
  , Relation
  , Type(..)
  , Function(..)
  , LabelId(..)
  ) where

import Data.Functor.Foldable.TH
import Eclair.Id
import Eclair.Pretty
import Eclair.RA.IndexSelection (Index)
import Eclair.LLVM.Metadata


type Relation = Id
type LowerBound = EIR
type UpperBound = EIR

data Type
  = Program
  | Value
  | Iter
  | Pointer Type
  | Void
  deriving (Eq, Show)

data Function
  = InitializeEmpty
  | Destroy
  | Purge
  | Swap
  | InsertRange
  | IsEmpty
  | Size
  | Contains
  | Insert
  | IterCurrent
  | IterNext
  | IterIsEqual
  | IterLowerBound
  | IterUpperBound
  | IterBegin
  | IterEnd
  deriving (Eq, Show)

newtype LabelId
  = LabelId Text
  deriving (Eq, Show)

instance IsString LabelId where
  fromString = LabelId . fromString

data EIR
  = Block [EIR]
  | Function Text [Type] Type EIR
  | FunctionArg Int
  | DeclareProgram [(Relation, Metadata)]
  | FieldAccess EIR Int
  | Var Text
  | Assign EIR EIR
  | Call Relation Index Function [EIR]
  | HeapAllocateProgram
  | FreeProgram EIR
  | StackAllocate Relation Index Type
  | Par [EIR]
  | Loop [EIR]
  | If EIR EIR
  | Not EIR
  | And EIR EIR
  | Equals EIR EIR
  | Jump LabelId
  | Label LabelId
  | Return EIR
  | Lit Word32
  deriving (Eq, Show)

makeBaseFunctor ''EIR


indentBlock :: Doc ann -> Doc ann -> Doc ann -> Doc ann
indentBlock begin end blk =
  nest indentation (begin <> hardline <> blk) <> hardline <> end

braceBlock :: Doc ann -> Doc ann
braceBlock = indentBlock "{" "}"

statementBlock :: Pretty a => [a] -> Doc ann
statementBlock = braceBlock . vsep . map pretty

instance Pretty Type where
  pretty = \case
    Program -> "Program"
    Value -> "Value"
    Iter -> "Iter"
    Pointer ty -> "*" <> pretty ty
    Void -> "Void"

instance Pretty Function where
  pretty = \case
    InitializeEmpty -> "init_empty"
    Destroy -> "destroy"
    Purge -> "purge"
    Swap -> "swap"
    InsertRange -> "insert_range"
    IsEmpty -> "is_empty"
    Contains -> "contains"
    Insert -> "insert"
    IterCurrent -> "iter_current"
    IterNext -> "iter_next"
    IterIsEqual -> "iter_is_equal"
    IterLowerBound -> "iter_lower_bound"
    IterUpperBound -> "iter_upper_bound"
    IterBegin -> "iter_begin"
    IterEnd -> "iter_end"
    Size -> "size"

instance Pretty LabelId where
  pretty (LabelId label) = pretty label

instance Pretty EIR where
  pretty = \case
    Block stmts ->
      statementBlock stmts
    Function name tys retTy body ->
      vsep ["fn" <+> pretty name <> parens (withCommas $ map pretty tys) <+> "->" <+> pretty retTy
           , pretty body -- Note: This is already a Block
           ]
    FunctionArg pos -> "FN_ARG" <> brackets (pretty pos)
    DeclareProgram metadatas ->
      vsep ["declare_type" <+> "Program"
           , braceBlock . vsep $
             map (\(r, meta) -> pretty r <+> pretty meta) metadatas
           ]
    FieldAccess ptr pos ->
      pretty ptr <> "." <> pretty pos
    Var v -> pretty v
    Assign var value ->
      pretty var <+> "=" <+> pretty value
    Call r _idx fn args ->
      pretty r <> "." <> pretty fn <> parens (withCommas $ map pretty args)
    HeapAllocateProgram ->
      "heap_allocate_program"
    FreeProgram ptr ->
      "free_program" <> parens (pretty ptr)
    StackAllocate r _idx ty ->
      pretty r <> "." <> "stack_allocate" <+> pretty ty
    Par stmts ->
      vsep ["parallel", statementBlock stmts]
    Loop stmts ->
      vsep ["loop", statementBlock stmts]
    If cond body ->
      let wrap = case body of
            Block stmts -> identity
            _ -> braceBlock
       in vsep ["if" <+> parens (pretty cond), wrap (pretty body)]
    Not bool ->
      "not" <+> pretty bool
    And bool1 bool2 ->
      pretty bool1 <+> "&&" <+> pretty bool2
    Equals lhs rhs ->
      pretty lhs <+> "==" <+> pretty rhs
    Jump label ->
      "goto" <+> pretty label
    Label label ->
      pretty label <> colon
    Return value ->
      "return" <+> pretty value
    Lit x -> pretty x
