{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.LSP.JSONSpec
  ( module Test.Eclair.LSP.JSONSpec
  ) where

import qualified Data.Hermes as H
import Eclair.Common.Location
import Eclair.LSP.Types
import Eclair.LSP.JSON
import Eclair.LSP.Handlers
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
    it "can encode response to JSON" $ do
      encodeJSON (responseToJSON (HoverResponse (HoverOk (SourceSpan "/etc/passwd" (SourcePos 11 13) (SourcePos 17 19)) U32)))
        `shouldBe` [text|
          {"type":"success","hover":{"location":{"file":"/etc/passwd","start":{"line":11,"column":13},"end":{"line":17,"column":19}},"type":"u32"}}
        |]
      encodeJSON (responseToJSON (HoverResponse (HoverError "/etc/passwd" (SourcePos 11 13) "sample hover error message")))
        `shouldBe` [text|
          {"type":"error","error":{"file":"/etc/passwd","position":{"line":11,"column":13},"message":"sample hover error message"}}
        |]
      encodeJSON (responseToJSON (DocumentHighlightResponse (DocHLOk [SourceSpan "/etc/passwd" (SourcePos 11 13) (SourcePos 17 19)])))
        `shouldBe` [text|
          {"type":"success","highlights":[{"file":"/etc/passwd","start":{"line":11,"column":13},"end":{"line":17,"column":19}}]}
        |]
      encodeJSON (responseToJSON (DocumentHighlightResponse (DocHLError "/etc/passwd" (SourcePos 11 13) "sample highlight error message")))
        `shouldBe` [text|
          {"type":"error","error":{"file":"/etc/passwd","position":{"line":11,"column":13},"message":"sample highlight error message"}}
        |]
      encodeJSON (responseToJSON (DiagnosticsResponse (DiagnosticsOk [Diagnostic Parser (SourceSpan "/etc/passwd" (SourcePos 11 13) (SourcePos 17 19)) Error "sample diagnostic message"])))
        `shouldBe` [text|
          {"type":"success","diagnostics":[{"location":{"file":"/etc/passwd","start":{"line":11,"column":13},"end":{"line":17,"column":19}},"source":"Parser","severity":"error","message":"sample diagnostic message"}]}
        |]
      encodeJSON (responseToJSON (DiagnosticsResponse (DiagnosticsError "/etc/passwd" (Just (SourcePos 11 13)) "sample diagnostic error message")))
        `shouldBe` [text|
          {"type":"error","error":{"file":"/etc/passwd","position":{"line":11,"column":13},"message":"sample diagnostic error message"}}
        |]
      encodeJSON (responseToJSON (DiagnosticsResponse (DiagnosticsError "/etc/passwd" Nothing "sample diagnostic error message")))
        `shouldBe` [text|
          {"type":"error","error":{"file":"/etc/passwd","position":{"line":0,"column":0},"message":"sample diagnostic error message"}}
        |]
      encodeJSON (responseToJSON SuccessResponse)
        `shouldBe` [text|
          {"success":true}
        |]
      encodeJSON (responseToJSON ShuttingDown)
        `shouldBe` [text|
          {"shutdown":true}
        |]

    it "can encode diagnostic to JSON" $ do
      encodeJSON (diagnosticToJSON (Diagnostic Parser (SourceSpan "/etc/passwd" (SourcePos 11 13) (SourcePos 17 19)) Error "sample diagnostic message"))
        `shouldBe` [text|
          {"location":{"file":"/etc/passwd","start":{"line":11,"column":13},"end":{"line":17,"column":19}},"source":"Parser","severity":"error","message":"sample diagnostic message"}
        |]

    it "can encode diagnostic source to JSON" $ do
      encodeJSON (diagnosticSourceToJSON Parser)
        `shouldBe` [text|
          "Parser"
        |]
      encodeJSON (diagnosticSourceToJSON Typesystem)
        `shouldBe` [text|
          "Typesystem"
        |]
      encodeJSON (diagnosticSourceToJSON SemanticAnalysis)
        `shouldBe` [text|
          "SemanticAnalysis"
        |]
      encodeJSON (diagnosticSourceToJSON Transpiler)
        `shouldBe` [text|
          "Transpiler"
        |]

    it "can encode severity to JSON" $ do
      encodeJSON (severityToJSON Error)
        `shouldBe` [text|
          "error"
        |]

    it "can encode source span to JSON" $ do
      encodeJSON (srcSpanToJSON (SourceSpan "/etc/passwd" (SourcePos 11 13) (SourcePos 17 19)))
        `shouldBe` [text|
          {"file":"/etc/passwd","start":{"line":11,"column":13},"end":{"line":17,"column":19}}
        |]

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
    it "can decode hover command from JSON" $ do
      decodesAs
        commandDecoder
        [text|
          {
            "type": "hover",
            "command": {
              "position": {"line": 100, "column": 22},
              "file": "/tmp/file.eclair"
            }
          }
        |]
        (Hover "/tmp/file.eclair" (SourcePos 100 22))

    it "can decode document-highlight command from JSON" $ do
      decodesAs
        commandDecoder
        [text|
          {
            "type": "document-highlight",
            "command": {
              "position": {"line": 100, "column": 22},
              "file": "/tmp/file.eclair"
            }
          }
        |]
        (DocumentHighlight "/tmp/file.eclair" (SourcePos 100 22))

    it "can decode diagnostics command from JSON" $ do
      decodesAs
        commandDecoder
        [text|
          {
            "type": "diagnostics",
            "command": {
              "file": "/tmp/file.eclair"
            }
          }
        |]
        (Diagnostics "/tmp/file.eclair")

    it "can decode update-vfs command from JSON" $ do
      decodesAs
        commandDecoder
        [text|
          {
            "type": "update-vfs",
            "command": {
              "file": "/etc/passwd",
              "contents": "root:*:0:0:System Administrator:/var/root:/bin/sh"
            }
          }
        |]
        (UpdateVFS "/etc/passwd" "root:*:0:0:System Administrator:/var/root:/bin/sh")

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

    it "can decode a document highlight command from JSON" $ do
      decodesAs
        referencesDecoder
        [text|
          {
            "position": {"line": 100, "column": 22},
            "file": "/tmp/file.eclair"
          }
        |]
        (DocumentHighlight "/tmp/file.eclair" (SourcePos 100 22))

    it "can decode a diagnostics command from JSON" $ do
      decodesAs
        diagnosticsDecoder
        [text|
          {
            "file": "/etc/passwd"
          }
        |]
        (Diagnostics "/etc/passwd")

    it "can decode a update-vfs command from JSON" $ do
      decodesAs
        updateVfsDecoder
        [text|
          {
            "file": "/etc/passwd",
            "contents": "root:*:0:0:System Administrator:/var/root:/bin/sh"
          }
        |]
        (UpdateVFS "/etc/passwd" "root:*:0:0:System Administrator:/var/root:/bin/sh")

    it "can decode a source position from JSON" $ do
      decodesAs
        srcPosDecoder
        [text|{"line": 42, "column": 10}|]
        (SourcePos 42 10)
