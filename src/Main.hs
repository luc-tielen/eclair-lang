module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Control.Exception
import LLVM.Pretty
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
            EmitRA -> emitRA
            EmitEIR -> emitEIR
            EmitLLVM -> emitLLVM
      fn file `catch` handleErrors
