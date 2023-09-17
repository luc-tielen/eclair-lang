{-# LANGUAGE LinearTypes, MagicHash #-}
-- | Helper module encoding Haskell values as JSON.
--   Only a limited set of functionality is provided.
--   (Needed since hermes-json only does decoding of JSON.)
module Eclair.JSON
  ( JSON(..)
  , encodeJSON
  ) where

import Data.Text.Builder.Linear.Buffer
import GHC.Prim (Addr#)

data JSON
  = Null
  | Boolean Bool
  | Number Int
  | String Text
  | Object [(Text, JSON)]
  | Array [JSON]

encodeJSON :: JSON -> Text
encodeJSON json =
  runBuffer (`toJSON'` json)
  where
    toJSON' :: Buffer %1 -> JSON -> Buffer
    toJSON' buf = \case
      Null ->
        buf |># "null"#
      Boolean b ->
        buf |># if b then "true"# else "false"#
      Number x ->
        buf |> show x
      String s ->
        dquotes buf (|> s)
      Object pairs ->
        braces buf (\buf' ->
          sepBy ","# buf' pairs (\buf'' (k, v) ->
            (dquotes buf'' (|> k) |>. ':') `toJSON'` v
          )
        )
      Array elems ->
        brackets buf (\buf' -> sepBy ","# buf' elems toJSON')

type BufferDecorator = Buffer %1 -> (Buffer %1 -> Buffer) -> Buffer

brackets :: BufferDecorator
brackets = betweenChars '[' ']'
{-# INLINABLE brackets #-}

braces :: BufferDecorator
braces = betweenChars '{' '}'
{-# INLINABLE braces #-}

dquotes :: BufferDecorator
dquotes = betweenChars '"' '"'
{-# INLINABLE dquotes #-}

betweenChars :: Char -> Char -> BufferDecorator
betweenChars begin end buf f =
  f (buf |>. begin) |>. end
{-# INLINABLE betweenChars #-}

sepBy :: forall a. Addr# -> Buffer %1 -> [a] -> (Buffer %1 -> a -> Buffer) -> Buffer
sepBy separator buf as f =
  foldlIntoBuffer combine buf parts
  where
    parts = intersperse Nothing $ map Just as
    combine :: Buffer %1 -> Maybe a -> Buffer
    combine buf' = \case
      Nothing ->
        buf' |># separator
      Just a ->
        f buf' a
{-# INLINABLE sepBy #-}
