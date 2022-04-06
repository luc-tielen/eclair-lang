module Main (main) where

import Protolude hiding ( Meta )
import qualified Data.Text.Lazy.IO as T
import LLVM.Analysis
import LLVM.Context
import LLVM.Module
import LLVM.Pretty
import LLVM.IRBuilder.Module
import Eclair.LLVM.BTree


{-
main =
  compile "tests/fixtures/codegen/index_selection.dl" >>= \case
    Left _ -> panic "Failed to compile to RA"
    Right ra -> putStrLn $ printRA ra
-}

main :: IO ()
main = do
  let meta = Meta { numColumns = 4
                  , index = [1, 3]
                  , blockSize = 256
                  , searchType = Linear
                  }
  moduleIR <- buildModuleT "btree" (codegen meta)
  withContext $ \ctx -> withModuleFromAST ctx moduleIR verify
  let output = ppllvm moduleIR
  T.putStrLn output
