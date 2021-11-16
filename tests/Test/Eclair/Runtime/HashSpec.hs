module Test.Eclair.Runtime.HashSpec
  ( module Test.Eclair.Runtime.HashSpec
  ) where

import Protolude
import Prelude (String)
import qualified Data.Set as Set
import Eclair.Runtime.Hash
import Test.Hspec


data MyOption = Option0 | Option1
  deriving stock Enum
  deriving ToHash via HashEnum MyOption

data Prefixed = Prefixed
  deriving stock Generic
  deriving ToHash via HashWithPrefix "prefix" Prefixed

data Config = Config Int Int Int MyOption
  deriving stock Generic
  deriving ToHash via HashWithPrefix "config" Config

data OnlyInt
  = OnlyInt
  { getInt :: Int
  , getText :: Text
  , getMyOption :: MyOption
  }
  deriving ToHash via HashOnly "getInt" OnlyInt

spec :: Spec
spec = describe "Hashing data" $ parallel $ do
  it "can hash ints" $ do
    unHash (getHash (1234 :: Int)) `shouldBe` "1234"

  it "can hash texts" $ do
    unHash (getHash ("12345" :: Text)) `shouldBe` "12345"

  it "can hash lists" $ do
    unHash (getHash (["123", "45", "6"] :: [Text])) `shouldBe` "123_45_6"

  it "can hash non empty lists" $ do
    let nonEmptyList = "123" :| ["45", "6"] :: NonEmpty Text
    unHash (getHash nonEmptyList) `shouldBe` "123_45_6"

  it "can hash sets" $ do
    let set = Set.fromList [123, 45, 6] :: Set Int
    -- NOTE: set is sorted => different ordering!
    unHash (getHash set) `shouldBe` "6_45_123"

  it "can hash sum types as enums" $ do
    unHash (getHash Option0) `shouldBe` "0"
    unHash (getHash Option1) `shouldBe` "1"

  it "can hash with a prefix" $ do
    unHash (getHash Prefixed) `shouldBe` "prefix__0"

  it "can hash generic product types" $ do
    let cfg = Config 1 2 3 Option1
    unHash (getHash cfg) `shouldBe` "config__1__2__3__1"

  it "can hash only a specific field of a record" $ do
    let onlyInt = OnlyInt 123 "abc" Option1
    unHash (getHash onlyInt) `shouldBe` "123"

  it "can combine hashes" $ do
    let x, y :: Int
        x = 42
        y = 1000
    unHash (getHash x <> getHash y) `shouldBe` "42__1000"
