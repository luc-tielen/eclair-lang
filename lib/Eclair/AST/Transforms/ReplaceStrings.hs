module Eclair.AST.Transforms.ReplaceStrings
  ( StringMap
  , transform
  ) where

import qualified Data.Map as Map
import Eclair.AST.IR
import Eclair.Transform


-- NOTE: "String" here means an eclair string!

type StringMap = Map Text Word32

transform :: Transform AST (AST, StringMap)
transform =
  Transform $ usingStateT mempty . cata rewrite
  where
    rewrite :: RewriteRuleT (StateT StringMap) AST
    rewrite = \case
      LitF nodeId (LString s) ->
        Lit nodeId . LNumber <$> replaceString s
      astf ->
        embed <$> sequence astf

replaceString :: Monad m => Text -> StateT StringMap m Word32
replaceString str = do
  value <- gets $ \strMap ->
    let count = fromIntegral $ length strMap
     in Map.findWithDefault count str strMap

  modify $ Map.insert str value
  pure value

