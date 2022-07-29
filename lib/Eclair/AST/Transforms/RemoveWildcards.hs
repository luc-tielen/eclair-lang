module Eclair.AST.Transforms.RemoveWildcards
  ( transform
  ) where

import qualified Data.Text as T
import Eclair.Id
import Eclair.AST.IR
import Data.Functor.Foldable hiding (fold)
import Eclair.Transform


transform :: Transform AST AST
transform =
  -- TODO: this adds extra state on top of nodeid state => proper mtl approach
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
