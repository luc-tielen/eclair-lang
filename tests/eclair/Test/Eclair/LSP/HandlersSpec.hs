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
  hoveringSpec

  describe "diagnostics" $ parallel $ do
    it "" pending

  describe "document highlight" $ parallel $ do
    it "" pending

hoveringSpec :: Spec
hoveringSpec = describe "Hover action" $ do
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

  -- TODO more error tests


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
