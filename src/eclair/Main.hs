module Main (main) where

import Control.Exception
import Eclair.ArgParser
import Eclair
import GHC.IO.Encoding
import System.Directory


tryReadFile :: FilePath -> IO (Maybe Text)
tryReadFile file = do
  fileExists <- doesFileExist file
  if fileExists
    then Just <$> readFileText file
    else pure Nothing

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
          params = Parameters (cpuTarget cfg) tryReadFile
      fn params file `catch` handleErrorsCLI
