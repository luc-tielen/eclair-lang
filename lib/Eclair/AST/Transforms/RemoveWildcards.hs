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
  Transform $ flip evalState 0 . cata rewrite
  where
    rewrite = \case
      PWildcardF nodeId -> do
        x <- get
        modify (+1)
        pure $ Var nodeId (Id $ "@wildcard_" <> show x)
      astf ->
        embed <$> sequence astf
