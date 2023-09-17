{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.LSP.JSONSpec
  ( module Test.Eclair.LSP.JSONSpec
  ) where

import qualified Data.Hermes as H
import Eclair.Common.Location
import Eclair.LSP.Types
import Eclair.LSP.JSON
import Eclair.JSON
import Eclair.TypeSystem
import Test.Hspec
import NeatInterpolation

decodesAs :: (Eq a, Show a) => H.Decoder a -> Text -> a -> IO ()
decodesAs decoder txt expected =
  H.decodeEither decoder (encodeUtf8 txt)
    `shouldBe` Right expected

spec :: Spec
spec = describe "LSP JSON processing" $ parallel $ do
  describe "JSON encoding" $ parallel $ do
    it "can encode response to JSON" pending

    it "can encode diagnostic to JSON" pending

    it "can encode diagnostic source to JSON" pending

    it "can encode severity to JSON" pending

    it "can encode source span to JSON" pending

    it "can encode source position to JSON" $ do
      encodeJSON (srcPosToJSON (SourcePos 32 58))
        `shouldBe` [text|
          {"line":32,"column":58}
        |]

    it "can encode type to JSON" $ do
      encodeJSON (typeToJSON U32) `shouldBe` [text|
        "u32"
      |]
      encodeJSON (typeToJSON Str) `shouldBe` [text|
        "string"
      |]

  describe "JSON decoding" $ parallel $ do
    it "can decode a command from JSON" pending

    it "can decode a hover command from JSON" $ do
      decodesAs
        hoverDecoder
        [text|
          {
            "position": {"line": 100, "column": 22},
            "file": "/tmp/file.eclair"
          }
        |]
        (Hover "/tmp/file.eclair" (SourcePos 100 22))

    it "can decode a document highlight command from JSON" pending

    it "can decode a diagnostics command from JSON" pending

    it "can decode a update-vfs command from JSON" pending

    it "can decode a source position from JSON" $ do
      decodesAs
        srcPosDecoder
        [text|{"line": 42, "column": 10}|]
        (SourcePos 42 10)
