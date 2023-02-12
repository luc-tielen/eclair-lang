module Main (main) where

import Eclair.ArgParser
import Eclair
import GHC.IO.Encoding
import System.Directory
import qualified Data.Text.IO as TIO
import System.Process
import Control.Exception
import System.IO.Error


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
          params = Parameters (cpuTarget cfg) tryReadFile
      whenLeftM_ (fn params file) $ \errs -> do
        let errActions =
              errs & map handleErrorsCLI
                   & intersperse (TIO.hPutStr stderr "\n")
        sequence_ errActions

    LSP -> do
      findExecutable "eclair-lsp-server" >>= \case
        Nothing -> do
          putTextLn "Could not find 'eclair-lsp-server'. Make sure it is in your PATH."
          exitFailure
        Just lspExe -> do
          callProcess lspExe [] `catch` (\(_ :: IOError) -> exitFailure)
