module Eclair.EIR.Printer ( Pretty, printEIR ) where

import Protolude
import Prettyprinter
import Prettyprinter.Render.Text
import Eclair.EIR.IR
import Eclair.Runtime.Metadata  -- TODO: declare pretty in that module
import qualified Data.Text as T

-- TODO: move to "PrinterUtils", refactor RA printer as well

printEIR :: EIR -> T.Text
printEIR = renderStrict . layoutSmart defaultLayoutOptions . pretty

indentation :: Int
indentation = 2

indentBlock :: Doc ann -> Doc ann -> Doc ann -> Doc ann
indentBlock begin end blk =
  -- nest indentation $ vsep [begin, blk, end]
  nest indentation (begin <> hardline <> blk) <> hardline <> end

braceBlock :: Doc ann -> Doc ann
braceBlock = indentBlock "{" "}"

statementBlock :: Pretty a => [a] -> Doc ann
statementBlock = braceBlock . vsep . map pretty

interleaveWith :: Doc ann -> [Doc ann] -> Doc ann
interleaveWith d = hsep . punctuate d

withCommas :: [Doc ann] -> Doc ann
withCommas = interleaveWith comma

between :: Doc ann -> Doc ann -> Doc ann -> Doc ann
between begin end doc =
  begin <> doc <> end

instance Pretty EIRType where
  pretty = \case
    Program -> "Program"
    Value -> "Value"
    Iter -> "Iter"
    Pointer ty -> "*" <> pretty ty

instance Pretty EIRFunction where
  pretty = \case
    InitializeEmpty -> "init_empty"
    Destroy -> "destroy"
    Purge -> "purge"
    Swap -> "swap"
    Merge -> "merge"
    IsEmpty -> "is_empty"
    Contains -> "contains"
    Insert -> "insert"
    IterCurrent -> "iter_current"
    IterNext -> "iter_next"
    IterIsEqual -> "iter_is_equal"
    IterLowerBound -> "iter_lower_bound"
    IterUpperBound -> "iter_upper_bound"

instance Pretty LabelId where
  pretty (LabelId label) = pretty label

instance Pretty EIR where
  pretty = \case
    Block stmts ->
      statementBlock stmts
    Function name tys body ->
      vsep ["fn" <+> pretty name <> parens (withCommas $ map pretty tys)
           , pretty body -- Note: This is already a Block
           ]
    FunctionArg pos -> "FN_ARG" <> brackets (pretty pos)
    DeclareType metadatas ->
      vsep ["declare_type" <+> "Program"
           , statementBlock metadatas
           ]
    FieldAccess ptr pos ->
      pretty ptr <> "." <> pretty pos
    Var v -> pretty v
    Assign var value ->
      pretty var <+> "=" <+> pretty value
    Call fn args ->
      pretty fn <> parens (withCommas $ map pretty args)
    HeapAllocateProgram ->
      "heap_allocate_program"
    FreeProgram ptr ->
      "free_program" <> parens (pretty ptr)
    StackAllocate ty r ->
      "stack_allocate" <+> pretty ty <+> between dquote dquote (pretty r)
    Par stmts ->
      vsep ["parallel", statementBlock stmts]
    Loop stmts ->
      vsep ["loop", statementBlock stmts]
    If cond body ->
      vsep ["if" <+> parens (pretty cond), braceBlock (pretty body)]
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
    -- TODO? RangeQuery r idx lb ub loopBody -> undefined
