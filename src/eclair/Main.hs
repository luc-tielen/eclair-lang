module Main (main) where

import Eclair.ArgParser
import Eclair.LSP
import Eclair
import GHC.IO.Encoding
import System.Directory
import qualified Data.Text.IO as TIO


tryReadFile :: FilePath -> IO (Maybe Text)
tryReadFile file = do
  fileExists <- doesFileExist file
  if fileExists
    then Just . decodeUtf8 <$> readFileBS file
    else pure Nothing

main :: IO ()
main = do
  setLocaleEncoding utf8
  arguments <- getArgs
  parseArgs arguments >>= \case
    Compile cfg -> do
      let file = mainFile cfg
          fn = case emitKind cfg of
            EmitTransformedAST -> emitTransformedAST
            EmitRA -> emitRA
            EmitTransformedRA -> emitTransformedRA
            EmitEIR -> emitEIR
            EmitLLVM -> emitLLVM
            EmitSouffle -> emitSouffle
          params = Parameters (numCores cfg) (cpuTarget cfg) tryReadFile
      whenLeftM_ (fn params file) $ \errs -> do
        let errActions =
              errs & map handleErrorsCLI
                   & intersperse (TIO.hPutStr stderr "\n")
        sequence_ errActions

    LSP ->
      lspMain
