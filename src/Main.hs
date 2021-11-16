module Main (main) where

import Protolude hiding ( Meta )
import qualified Data.Text.Lazy.IO as T
import qualified Data.Set as Set
import LLVM.Pretty
import LLVM.IRBuilder.Module
import Eclair.Runtime.BTree
import Eclair.RA.Printer
import Eclair


{-
main =
  compile "tests/fixtures/codegen/index_selection.dl" >>= \case
    Left _ -> panic "Failed to compile to RA"
    Right ra -> putStrLn $ printRA ra
-}

main :: IO ()
main = do
  let meta = Meta { numColumns = 4
                  , index = Set.fromList [1, 3]
                  , blockSize = 256
                  , searchType = Linear
                  }
  moduleIR <- buildModuleT "btree" (codegen meta)
  let output = ppllvm moduleIR
  T.putStrLn output
