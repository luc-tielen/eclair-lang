module Eclair.EIR.Printer ( Pretty ) where

import Protolude hiding (Type)
import Eclair.Pretty
import Eclair.EIR.IR
import Eclair.LLVM.Metadata
import qualified Data.Text as T

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
           , statementBlock metadatas
           ]
    FieldAccess ptr pos ->
      pretty ptr <> "." <> pretty pos
    Var v -> pretty v
    Assign var value ->
      pretty var <+> "=" <+> pretty value
    Call r idx fn args ->
      pretty r <> parens (pretty idx) <> "." <> pretty fn <> parens (withCommas $ map pretty args)
    HeapAllocateProgram ->
      "heap_allocate_program"
    FreeProgram ptr ->
      "free_program" <> parens (pretty ptr)
    StackAllocate r idx ty ->
      pretty r <> parens (pretty idx) <> "." <> "stack_allocate" <+> pretty ty
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
