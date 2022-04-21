module Main ( main ) where

import Protolude
import qualified Test.Eclair.LLVM.BTreeTest as BTree

-- This approach (2nd testsuite) feels a bit clunky, but needed since we only
-- want to use the JIT once for an entire test file.
-- (Not possible with hspec + hspec-discover)
-- NOTE: When more modules get added, this will print out 1 spec tree per file,
--       but should be fine as long as we results and if it exits on first failure.
main :: IO ()
main = sequence_
  [ BTree.spec
  ]

