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

  describe "diagnostics" $ parallel $ do
    it "" pending

hoverSpec :: Spec
hoverSpec = describe "Hover action" $ do
  it "reports types on hover" $ do
    let file = fixture "hover.eclair"
        srcPos1 = SourcePos 6 8  -- 1-indexed, same as editor!
        srcPos2 = SourcePos 12 10
    (result1, result2) <- withLSP (Just file) $ do
      (,) <$> hoverHandler file srcPos1
          <*> hoverHandler file srcPos2
    result1 `shouldBe`
      HoverOk (SourceSpan file (SourcePos 6 8) (SourcePos 6 9)) U32
    result2 `shouldBe`
      HoverOk (SourceSpan file (SourcePos 12 9) (SourcePos 12 14)) Str

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
        srcPos1 = SourcePos 7 11 -- x
        srcPos2 = SourcePos 7 14 -- y
        srcPos3 = SourcePos 8 11 -- z
    (result1, result2, result3) <- withLSP (Just file) $ do
      (,,) <$> documentHighlightHandler file srcPos1
           <*> documentHighlightHandler file srcPos2
           <*> documentHighlightHandler file srcPos3
    result1 `shouldBe` DocHLOk
      [ SourceSpan file (SourcePos 7 11) (SourcePos 7 12)
      , SourceSpan file (SourcePos 8 8) (SourcePos 8 9)
      ]
    result2 `shouldBe` DocHLOk
      [ SourceSpan file (SourcePos 7 14) (SourcePos 7 15)
      , SourceSpan file (SourcePos 9 16) (SourcePos 9 17)
      ]
    result3 `shouldBe` DocHLOk
      [ SourceSpan file (SourcePos 8 11) (SourcePos 8 12)
      , SourceSpan file (SourcePos 9 13) (SourcePos 9 14)
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
