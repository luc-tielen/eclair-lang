{-# LANGUAGE DuplicateRecordFields #-}
module Main ( main ) where

import Prelude hiding (head)
import Test.Hspec
import Language.LSP.Test
import Language.LSP.Types
    ( Diagnostic (..)
    , DiagnosticSeverity (..)
    , Hover (..)
    , HoverContents (..)
    , MarkupContent (..)
    , Position (..)
    )
import qualified GHC.IO.Encoding


baseDir :: FilePath -> FilePath
baseDir d = "tests/lsp/fixtures/" <> d

hoveringSpec :: FilePath -> Spec
hoveringSpec dir = describe "Hover action" $ do
  it "reports types on hover" $ do
    runSession "eclair-lsp" fullCaps dir $ do
      docId <- openDoc "hover.eclair" "eclair"
      let varXPos = Position 5 7
          literalPos = Position 11 10
          extractContents = map _contents
          getValue = toString . _value
      xHover <- getHover docId varXPos
      litHover <- getHover docId literalPos
      liftIO $ do
        case (extractContents xHover, extractContents litHover) of
          (Just (HoverContents xContent), Just (HoverContents litContent)) -> do
            getValue xContent `shouldBe` "u32"
            getValue litContent `shouldBe` "string"
          _ -> error "test failed"

diagnosticsSpec :: FilePath -> Spec
diagnosticsSpec fixtureDir = describe "Diagnostics action" $ parallel $ do
  it "reports invalid syntax" $ do
    runSession "eclair-lsp" fullCaps fixtureDir $ do
      _ <- openDoc "invalid_syntax.eclair" "eclair"
      [diag] <- waitForDiagnosticsSource "Eclair.Parser"
      liftIO $ _severity diag `shouldBe` Just DsError

  it "reports semantic errors" $ do
    runSession "eclair-lsp" fullCaps fixtureDir $ do
      _ <- openDoc "semantic_errors.eclair" "eclair"
      [diag] <- waitForDiagnosticsSource "Eclair.SemanticAnalysis"
      liftIO $ do
        _severity diag `shouldBe` Just DsError
        toString (_message diag) `shouldContain` "Unbound variable"

  it "reports type errors" $ do
    runSession "eclair-lsp" fullCaps fixtureDir $ do
      _ <- openDoc "type_errors.eclair" "eclair"
      [diag] <- waitForDiagnosticsSource "Eclair.Typesystem"
      liftIO $ do
        _severity diag `shouldBe` Just DsError
        toString (_message diag) `shouldContain` "Expression doesn't match annotation"

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  hspec $ do
    hoveringSpec (baseDir "hovering")
    diagnosticsSpec (baseDir "diagnostics")
