module Test.Eclair.Runtime.BTreeSpec
  ( module Test.Eclair.Runtime.BTreeSpec
  ) where

import Protolude
import Test.Hspec

{-
TODO:
specialize for 1 specific situation
JIT compile using LLVM
import foreign functions into haskell
make ADT for all operations
generate list of ops using hedgehog, run program with that
-}

spec :: Spec
spec = describe "btree" $ parallel $ do
  it "can create and destroy btrees" pending

  it "can iterate over the full range of values" pending

  it "can use lower- and upper-bound to iterate over a subset of values" pending

  -- TODO: properties
  it "should be empty after purging" $ do
    pending

  describe "swap" $ parallel $ do
    it "swaps contents of tree A and B" pending

    it "is a no-op to swap twice" pending

  describe "insert" $ parallel $ do
    it "is not empty afterwards" pending

    it "does nothing if value is already stored in tree" pending

    it "adds the new value if not stored in tree" pending

    it "is commutative" pending

  describe "insertRange" $ parallel $ do
    it "increases in size by up to N when adding N elements" pending
    -- TODO same props as insert?

  describe "isEmpty" $ parallel $ do
    it "returns true for empty trees" pending

    it "returns false for non-empty trees" pending

  describe "contains" $ parallel $ do
    it "returns true if element is inside the tree" pending

    it "returns false if element is not inside the tree" pending
