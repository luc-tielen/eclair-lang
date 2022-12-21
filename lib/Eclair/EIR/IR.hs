{-# LANGUAGE TemplateHaskell #-}

module Eclair.EIR.IR
  ( EIR(..)
  , EIRF(..)
  , Relation
  , Op(..)
  , ConstraintOp(..)
  , Type(..)
  , Function(..)
  , LabelId(..)
  , Visibility(..)
  ) where

import Eclair.Id
import Eclair.Operator
import Eclair.Literal
import Eclair.Pretty
import Eclair.RA.IndexSelection (Index)
import Eclair.LLVM.Metadata


type Relation = Id

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
  | InsertRange Relation Index  -- InsertRange specialized for this relation and index
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

data Op
  = RelationOp Relation Index Function  -- a primop related to operations on relations
  | SymbolTableInit
  | SymbolTableDestroy
  | SymbolTableInsert
  | ComparisonOp ConstraintOp
  deriving (Eq, Show)

data Visibility
  = Public
  | Private
  deriving (Eq, Show)

data EIR
  = Module [EIR]  -- A module is the same as a block, but is rendered in a different way.
  | Block [EIR]
  | Function Visibility Text [Type] Type EIR
  | FunctionArg Int
  | DeclareProgram [(Relation, Metadata)]
  | FieldAccess EIR Int
  | Var Text
  | Assign EIR EIR
  | PrimOp Op [EIR]                     -- A primitive operation, these tend to be simple function calls or operators
  | HeapAllocateProgram
  | FreeProgram EIR
  | StackAllocate Relation Index Type
  | Par [EIR]
  | Loop [EIR]
  | If EIR EIR
  | Not EIR
  | And EIR EIR
  | Jump LabelId
  | Label LabelId
  | Return EIR
  | Lit Literal
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
    InsertRange r idx -> "insert_range" <> angles (pretty r <> pretty idx)
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

instance Pretty Op where
  pretty = \case
    SymbolTableInit ->
      "symbol_table.init"
    SymbolTableDestroy ->
      "symbol_table.destroy"
    SymbolTableInsert ->
      "symbol_table.insert"
    RelationOp r _idx fn ->
      pretty r <> "." <> pretty fn
    ComparisonOp op ->
      -- Since `=` is already used for assignment in EIR, we use `==` for comparison.
      if op == Equals then "==" else pretty op

instance Pretty EIR where
  pretty = \case
    Module stmts ->
      -- This adds newlines in between top level EIR statements
      vsep $ intersperse mempty $ map pretty stmts
    Block stmts ->
      statementBlock stmts
    Function visibility name tys retTy body ->
      let fn = if visibility == Public
                 then "export fn"
                 else "fn"
       in vsep [ fn <+> pretty name <> parens (withCommas $ map pretty tys) <+> "->" <+> pretty retTy
               , pretty body -- Note: This is already a Block
               ]
    FunctionArg pos -> "FN_ARG" <> brackets (pretty pos)
    DeclareProgram metadatas ->
      vsep [ "declare_type" <+> "Program"
           , braceBlock . vsep $
               "symbol_table" : map (\(r, meta) -> pretty r <+> pretty meta) metadatas
           ]
    FieldAccess ptr pos ->
      pretty ptr <> "." <> pretty pos
    Var v -> pretty v
    Assign var value ->
      pretty var <+> "=" <+> pretty value
    PrimOp op@(ComparisonOp {}) [arg1, arg2] ->
      pretty arg1 <+> pretty op <+> pretty arg2
    PrimOp op args ->
      pretty op <> parens (withCommas $ map pretty args)
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
            Block _ -> identity
            _ -> braceBlock
       in vsep ["if" <+> parens (pretty cond), wrap (pretty body)]
    Not bool' ->
      "not" <+> pretty bool'
    And bool1 bool2 ->
      pretty bool1 <+> "&&" <+> pretty bool2
    Jump label ->
      "goto" <+> pretty label
    Label label ->
      pretty label <> colon
    Return value ->
      "return" <+> pretty value
    Lit x -> pretty x
