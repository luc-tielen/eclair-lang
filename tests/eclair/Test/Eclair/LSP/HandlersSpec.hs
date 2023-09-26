module Test.Eclair.LSP.HandlersSpec
  ( module Test.Eclair.LSP.HandlersSpec
  ) where

import Eclair
import Eclair.TypeSystem
import Eclair.Common.Location
import Eclair.LSP.Monad
import Eclair.LSP.Handlers
import qualified Data.Map as M
import Test.Hspec

spec :: Spec
spec = describe "LSP handlers" $ parallel $ do
  hoverSpec
  documentHighlightSpec
  diagnosticsSpec

hoverSpec :: Spec
hoverSpec = describe "Hover action" $ do
  it "reports types on hover" $ do
    let file = fixture "hover.eclair"
        srcPos1 = SourcePos 5 7  -- 0-indexed!
        srcPos2 = SourcePos 11 9
    (result1, result2) <- withLSP (Just file) $ do
      (,) <$> hoverHandler file srcPos1
          <*> hoverHandler file srcPos2
    result1 `shouldBe`
      HoverOk (SourceSpan file (SourcePos 5 7) (SourcePos 5 8)) U32
    result2 `shouldBe`
      HoverOk (SourceSpan file (SourcePos 11 8) (SourcePos 11 13)) Str

  it "returns an error if file not found in vfs" $ do
    let file = "not_found.eclair"
        srcPos = SourcePos 11 10
    result <- withLSP Nothing $ hoverHandler file srcPos
    result `shouldBe` HoverError file (SourcePos 11 10) "File not found in VFS!"

  it "returns an error if no hover information available for position" $ do
    let file = fixture "hover.eclair"
        srcPos1 = SourcePos 4 1  -- whitespace
        srcPos2 = SourcePos 14 10  -- outside of file bounds
    (result1, result2) <- withLSP (Just file) $ do
      (,) <$> hoverHandler file srcPos1
          <*> hoverHandler file srcPos2
    result1 `shouldBe`
      HoverError file srcPos1 "No type information for this position!"
    result2 `shouldBe`
      HoverError file srcPos2 "Error computing location offset in file!"

  it "returns an error if file failed to parse" $ do
    let file = fixture "unparsable.eclair"
        srcPos = SourcePos 4 1
    result <- withLSP (Just file) $ hoverHandler file srcPos
    result `shouldBe` HoverError file srcPos "File contains errors!"

  it "returns an error if file failed to typecheck" $ do
    let file = fixture "type_errors.eclair"
        srcPos = SourcePos 4 1
    result <- withLSP (Just file) $ hoverHandler file srcPos
    result `shouldBe` HoverError file srcPos "File contains errors!"

documentHighlightSpec :: Spec
documentHighlightSpec = describe "Document highlight action" $ do
  it "highlights the same identifiers in scope" $ do
    let file = fixture "document_highlight.eclair"
        srcPos1 = SourcePos 6 10 -- x
        srcPos2 = SourcePos 6 13 -- y
        srcPos3 = SourcePos 7 10 -- z
    (result1, result2, result3) <- withLSP (Just file) $ do
      (,,) <$> documentHighlightHandler file srcPos1
           <*> documentHighlightHandler file srcPos2
           <*> documentHighlightHandler file srcPos3
    result1 `shouldBe` DocHLOk
      [ SourceSpan file (SourcePos 6 10) (SourcePos 6 11)
      , SourceSpan file (SourcePos 7 7) (SourcePos 7 8)
      ]
    result2 `shouldBe` DocHLOk
      [ SourceSpan file (SourcePos 6 13) (SourcePos 6 14)
      , SourceSpan file (SourcePos 8 15) (SourcePos 8 16)
      ]
    result3 `shouldBe` DocHLOk
      [ SourceSpan file (SourcePos 7 10) (SourcePos 7 11)
      , SourceSpan file (SourcePos 8 12) (SourcePos 8 13)
      ]

  it "returns an error if file not found in vfs" $ do
    let file = "not_found.eclair"
        srcPos = SourcePos 11 10
    result <- withLSP Nothing $ documentHighlightHandler file srcPos
    result `shouldBe` DocHLError file (SourcePos 11 10) "File not found in VFS!"

  it "returns an error if file failed to parse" $ do
    let file = fixture "unparsable.eclair"
        srcPos = SourcePos 4 1
    result <- withLSP (Just file) $ documentHighlightHandler file srcPos
    result `shouldBe` DocHLError file srcPos "Failed to get highlight information!"

diagnosticsSpec :: Spec
diagnosticsSpec = describe "Diagnostics action" $ parallel $ do
  it "reports nothing if file is OK" pending

  it "reports invalid syntax" $ do
    let file = fixture "invalid_syntax.eclair"
    DiagnosticsOk diags <- withLSP (Just file) $ diagnosticsHandler file
    length diags `shouldBe` 2

  it "returns an error if file not found in vfs" $ do
    let file = "not_found.eclair"
    result <- withLSP Nothing $ diagnosticsHandler file
    result `shouldBe` DiagnosticsError file Nothing "File not found in VFS!"

  it "reports semantic errors" $ do
    let file = fixture "semantic_errors.eclair"
    DiagnosticsOk [diag] <- withLSP (Just file) $ diagnosticsHandler file
    let (Diagnostic _ _ _ msg) = diag
    toString msg `shouldContain` "Wildcard in top level fact"

  it "reports type errors" $ do
    let file = fixture "type_errors.eclair"
    DiagnosticsOk (_:_:diag:_) <- withLSP (Just file) $ diagnosticsHandler file
    let (Diagnostic _ _ _ msg) = diag
    toString msg `shouldContain` "Type mismatch"

fixture :: FilePath -> FilePath
fixture file =
  "./tests/eclair/fixtures/lsp/" <> file

withLSP :: Maybe FilePath -> LspM a -> IO a
withLSP mFile m = runLSP $ do
  case mFile of
    Nothing -> pass
    Just file -> do
      fileContents <- decodeUtf8 <$> readFileBS file
      lift $ vfsSetFile file fileContents

  vfsVar <- lift getVfsVar
  let readVFS path = do
        vfs <- readMVar vfsVar
        pure $! M.lookup path vfs
  let params = Parameters 1 Nothing readVFS
  local (const params) m
