module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Control.Exception
import LLVM.Pretty
import Eclair


main :: IO ()
main = do
  arguments <- getArgs
  case nonEmpty arguments of
    Nothing -> panic "Expected usage: 'eclairc FILE'"
    Just args -> do
      let filePath = head args
      --(print =<< semanticAnalysis filePath) `catch` handleErrors
      emitLLVM filePath `catch` handleErrors
