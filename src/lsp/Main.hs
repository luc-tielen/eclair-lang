module Main (main) where

import GHC.IO.Encoding
import Eclair.LSP


main :: IO ()
main = do
  setLocaleEncoding utf8
  run (Just "/tmp/eclair-lsp.log")
