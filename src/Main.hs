module Main ( main ) where

import Protolude hiding ( Meta )
import qualified Data.Text.Lazy.IO as T
import qualified Data.Set as Set
import LLVM.Pretty
import LLVM.IRBuilder.Module
import Eclair.Runtime.BTree


main :: IO ()
main = do
  let meta = Meta { arch = X64
                  , numColumns = 4
                  , blockSize = 256
                  , index = Set.fromList [1, 3]
                  , searchType = Linear
                  }
  let moduleIR = buildModule "btree" (codegen meta)
      output = ppllvm moduleIR
  T.putStrLn output

