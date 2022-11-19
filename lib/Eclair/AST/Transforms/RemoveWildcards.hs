module Eclair.AST.Transforms.RemoveWildcards
  ( transform
  ) where

import Eclair.Id
import Eclair.AST.IR
import Eclair.Transform


transform :: Transform AST AST
transform =
  Transform $ flip evalStateT 0 . cata rewrite
  where
    rewrite :: RewriteRuleT (StateT Int) AST
    rewrite = \case
      PWildcardF nodeId -> do
        x <- get
        modify (+1)
        pure $ Var nodeId (Id $ "@wildcard_" <> show x)
      astf ->
        embed <$> sequence astf
