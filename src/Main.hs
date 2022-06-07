module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Control.Exception
import LLVM.Pretty
import Eclair.ArgParser
import Eclair


main :: IO ()
main = do
  arguments <- getArgs
  parseArgs arguments >>= \case
    Compile cfg -> do
      let file = mainFile cfg
          fn = case emitKind cfg of
            EmitRA -> emitRA
            EmitEIR -> emitEIR
            EmitLLVM -> emitLLVM
      fn file `catch` handleErrors
