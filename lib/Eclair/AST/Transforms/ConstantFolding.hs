module Eclair.AST.Transforms.ConstantFolding
  ( transform
  ) where

import Eclair.AST.IR
import Eclair.Transform

transform :: Transform AST AST
transform = pureTransform $ cata $ \case
  BinOpF _ op (Lit lhsId (LNumber lhs)) (Lit _ (LNumber rhs)) ->
    let opFn = case op of
          Plus -> (+)
          Minus -> (-)
          Multiply -> (*)
          Divide -> div
    in Lit lhsId $ LNumber $ opFn lhs rhs
  ast ->
    embed ast
