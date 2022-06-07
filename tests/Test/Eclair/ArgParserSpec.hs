module Test.Eclair.ArgParserSpec
  ( module Test.Eclair.ArgParserSpec
  ) where

import Test.Hspec
import qualified Data.Text as T
import Eclair.ArgParser
import Control.Exception
import System.IO.Silently
import System.Exit


parseArgs' :: Text -> IO Config
parseArgs' args =
  parseArgs (map toString $ T.split (== ' ') args)

shouldFail :: IO a -> IO ()
shouldFail m = hSilence [stderr] $ do
  void m `catch` handler
  where
    handler = \case
      ExitFailure 1 -> pass
      e -> panic $ "Unknown error: " <> show e


spec :: Spec
spec = describe "argument parsing" $ do
  describe "compile mode" $ parallel $ do
    it "supports 'compile' as the command" $ do
      cfg <- parseArgs' "compile test.dl"
      cfg `shouldBe` Compile (CompileConfig "test.dl" EmitLLVM)

    it "supports 'c' as the command" $ do
      cfg <- parseArgs' "c test.dl"
      cfg `shouldBe` Compile (CompileConfig "test.dl" EmitLLVM)

    it "supports no other commands" $ do
      shouldFail $ parseArgs' "unknown"
      shouldFail $ parseArgs' "unknown arg1"

    it "requires a main file" $ do
      shouldFail $ parseArgs' "c"

    it "supports emitting RA" $ do
      for_ ["ra", "RA"] $ \ra -> do
        cfg <- parseArgs' $ "c test.dl --emit " <> ra
        cfg `shouldBe` Compile (CompileConfig "test.dl" EmitRA)

    it "supports emitting EIR" $ do
      for_ ["eir", "EIR"] $ \eir -> do
        cfg <- parseArgs' $ "c test.dl --emit " <> eir
        cfg `shouldBe` Compile (CompileConfig "test.dl" EmitEIR)

    it "supports emitting LLVM IR" $ do
      for_ ["llvm", "LLVM"] $ \llvm -> do
        cfg <- parseArgs' $ "c test.dl --emit " <> llvm
        cfg `shouldBe` Compile (CompileConfig "test.dl" EmitLLVM)

    it "does not support emitting anything else" $ do
      shouldFail $ parseArgs' "c test.dl --emit unknown-ir"

    it "defaults to emitting LLVM IR" $ do
      cfg <- parseArgs' "c test.dl"
      cfg `shouldBe` Compile (CompileConfig "test.dl" EmitLLVM)
