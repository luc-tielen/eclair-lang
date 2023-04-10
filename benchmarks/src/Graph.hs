{-# LANGUAGE BangPatterns #-}
module Graph
  ( Graph(..), parseGraph
  ) where

import Facts
import Data.Attoparsec.Text
import Data.Word
import Data.Vector (Vector)
import Data.Text (Text)
import qualified Data.Vector as V

data Graph = Graph { vertices :: Vector Vertex, edges :: Vector Edge }

commentLine :: Parser ()
commentLine = do
  _ <- char 'c'
  skipWhile (/= '\n')
  _ <- char '\n'
  pure ()

problemLine :: Parser Word32
problemLine = do
  _ <- char 'p'
  skipSpace
  skipWhile (/= ' ')
  skipSpace
  x <- decimal
  skipWhile (/= '\n')
  _ <- char '\n'
  pure x

edgeLine :: Parser Edge
edgeLine = do
  _ <- char 'a'
  skipSpace
  eFrom <- decimal
  skipSpace
  eTo <- decimal
  skipSpace
  skipWhile (/= '\n')
  _ <- char '\n'
  pure $ Edge eFrom eTo

parser :: Parser Graph
parser = do
  skipMany commentLine
  numPoints <- problemLine
  !es <- V.fromList <$> manyTill edgeLine endOfInput
  let !vs = V.map Vertex $ V.fromList [1..numPoints + 1]
  pure $! Graph { vertices = vs, edges = es }

parseGraph :: Text -> IO Graph
parseGraph txt =
  case parseOnly parser txt of
    Left err ->
      error $ "Failed to parse graph: " <> err
    Right graph ->
      pure graph
