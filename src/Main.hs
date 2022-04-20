module Main (main) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy.IO as T
import LLVM.Pretty
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
        Right llvmModule -> do
          T.putStrLn $ ppllvm llvmModule
