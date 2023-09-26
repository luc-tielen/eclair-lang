{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.JSONSpec
  ( module Test.Eclair.JSONSpec
  ) where

import Eclair.JSON
import Test.Hspec
import NeatInterpolation

spec :: Spec
spec = describe "JSON encoding" $ parallel $ do
  it "can encode null" $ do
    encodeJSON Null `shouldBe` "null"

  it "can encode booleans" $ do
    encodeJSON (Boolean True) `shouldBe` "true"
    encodeJSON (Boolean False) `shouldBe` "false"

  it "can encode strings" $ do
    encodeJSON (String "abcdef") `shouldBe` [text|"abcdef"|]
    encodeJSON (String "123") `shouldBe` [text|"123"|]

  it "can encode integers" $ do
    encodeJSON (Number 42) `shouldBe` "42"
    encodeJSON (Number 123) `shouldBe` "123"

  it "can encode objects" $ do
    encodeJSON (Object [("line", Number 10), ("column", Number 33)]) `shouldBe` [text|
      {"line":10,"column":33}
    |]
    encodeJSON (Object [("a", Null), ("b", Boolean True)]) `shouldBe` [text|
      {"a":null,"b":true}
    |]

  it "can encode arrays" $ do
    encodeJSON (Array []) `shouldBe` "[]"
    encodeJSON (Array [Number 123, String "abc", Null]) `shouldBe` [text|
      [123,"abc",null]
    |]
