module Main (main) where

import Protolude hiding ( Meta )
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy.IO as T
import LLVM.Analysis
import LLVM.Context
import LLVM.Module
import LLVM.Pretty
import LLVM.IRBuilder.Module
import Eclair


main :: IO ()
main = do
  arguments <- getArgs
  case nonEmpty arguments of
    Nothing -> panic "Expected usage: 'eclairc FILE'"
    Just args -> do
      let filePath = NE.head args
      compile filePath >>= \case
        Left _ -> panic "Failed to compile the Datalog code!"
        Right llvmModule -> putStrLn $ ppllvm llvmModule
