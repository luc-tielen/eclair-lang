module Main (main) where

import Control.Exception
import Eclair.ArgParser
import Eclair
import GHC.IO.Encoding


main :: IO ()
main = do
  setLocaleEncoding utf8
  arguments <- getArgs
  parseArgs arguments >>= \case
    Compile cfg -> do
      let file = mainFile cfg
          fn = case emitKind cfg of
            EmitSimplifiedAST -> emitSimplifiedAST
            EmitRA -> emitRA
            EmitEIR -> emitEIR
            EmitLLVM -> emitLLVM
      fn (cpuTarget cfg) file `catch` handleErrors
