module Main (main) where

import qualified Data.Text.Lazy.IO as T
import Control.Exception
import LLVM.Pretty
import qualified Eclair.AST.Analysis as SA
import Eclair.ArgParser
import Eclair
import GHC.IO.Encoding


-- TODO: remove this hack, should be integrated into main compiler line.
-- this is only needed atm since main compiler pipeline doesn't use the actual semantic analysis results yet.
manualSemanticAnalysis :: FilePath -> IO ()
manualSemanticAnalysis file = do
  result <- semanticAnalysis file
  liftIO $ forM_ (SA.maybeToSemanticError result) (throwIO . SemanticErr)

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
      handle handleErrors $ do
        manualSemanticAnalysis file
        fn file
