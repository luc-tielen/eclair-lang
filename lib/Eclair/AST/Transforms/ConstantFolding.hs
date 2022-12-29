module Eclair.AST.Transforms.ConstantFolding
  ( transform
  ) where

import Eclair.AST.IR
import Eclair.Transform

transform :: Transform AST AST
transform = pureTransform $ cata $ \case
  BinOpF nodeId op (Lit _ (LNumber lhs)) (Lit _ (LNumber rhs)) ->
    let opFn = case op of
          Plus -> (+)
          Minus -> (-)
          Multiply -> (*)
          Divide -> div
    in Lit nodeId $ LNumber $ opFn lhs rhs
  ast ->
    embed ast
