{-# LANGUAGE TypeFamilies, RankNTypes, QuasiQuotes #-}

module Test.Eclair.EIR.CodegenSpec
  ( module Test.Eclair.EIR.CodegenSpec
  ) where

import Protolude hiding ((<.>))
import Control.Arrow ((&&&))
import qualified Data.Text as T
import Eclair
import qualified Eclair.EIR.IR as EIR
import Eclair.EIR.Printer
import Eclair.Syntax
import System.FilePath
import Test.Hspec
import NeatInterpolation


cg :: FilePath -> IO T.Text
cg path = do
  let file = "tests/fixtures/codegen" </> path <.> "dl"
  result <- compile file
  case result of
    Left err -> panic $ "Failed to parse " <> T.pack file <> "!"
    Right eir -> pure $ printEIR eir

resultsIn :: IO T.Text -> T.Text -> IO ()
resultsIn action output = do
  result <- action
  result `shouldBe` T.strip output


extractDeclTypeSnippet :: Text -> Text
extractDeclTypeSnippet result = extractedSnippet
  where
    extractedSnippet = T.strip $ T.unlines $ reAddHeader $ fixIndent $ extract endLineNr
    extract end = take (endLineNr - 3) $ drop 3 lines
    reAddHeader body = ["declare_type Program", "{"] ++ body ++ ["}"]
    fixIndent = map (("  " <>) . T.stripStart)
    lines = T.lines result
    lineCount = length lines
    endTypeLineNr = find ((T.isPrefixOf "}" . T.stripStart) . snd) $ zip [0..] lines
    endLineNr = fromMaybe lineCount (map fst endTypeLineNr)

data Region
  = Region
  { regionName :: Maybe Text
  , regionBegin :: Int
  , regionEnd :: Int
  }

extractFnSnippet :: Text -> Text -> Maybe Text
extractFnSnippet result fnSignature =
  uncurry extractSnippet <$> matchingRegion
  where
    extractSnippet begin end =
      let fnBody = map fixIndent $ stripFnHeader begin end $ T.lines result
       in T.strip $ T.unlines $ reAddFnHeader fnBody
    -- 2 and 3 is to strip fn ... + {}
    stripFnHeader begin end = take (end - begin - 3) . drop (begin + 2)
    reAddFnHeader body = ["fn " <> fnSignature, "{"] ++ body ++ ["}"]
    fixIndent = ("  " <>) . T.stripStart  -- because top most block is stripped off
    lines = zip [0..] $ T.lines result
    lineCount = length lines
    relevantLines = filter (T.isPrefixOf "fn" . T.stripStart . snd) lines
    parsedLines = map (map (head . drop 1 . T.words)) relevantLines
    endLineNrs = drop 1 (map fst parsedLines) ++ [lineCount - 1]
    regions = zipWith (\(start, fn) end -> Region fn start end) parsedLines endLineNrs
    matchingRegion = (regionBegin &&& regionEnd) <$> find ((== Just fnSignature) . regionName) regions

spec :: Spec
spec = describe "EIR Code Generation" $ parallel $ do
  fit "generates code for a single fact" $ do
    eir <- cg "single_fact"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        btree(num_columns=3, index=[0,1,2], block_size=256, search_type=linear)
        btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init()" `shouldBe` Just [text|
      fn eclair_program_init()
      {
        program = heap_allocate_program
        init_empty(program.0)
        init_empty(program.1)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_init()" `shouldBe` Just [text|
      fn eclair_program_init()
      {
        program = heap_allocate_program
        init_empty(program.0)
        init_empty(program.1)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program)" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program)
      {
        destroy(FN_ARG[0].0)
        destroy(FN_ARG[0].1)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program)" `shouldBe` Just [text|
      fn eclair_program_run(*Program)
      {
        value = stack_allocate Value "another"
        value.0 = 1
        value.1 = 2
        value.2 = 3
        insert(FN_ARG[0].0, value)
        value = stack_allocate Value "edge"
        value.0 = 2
        value.1 = 3
        insert(FN_ARG[0].1, value)
        value = stack_allocate Value "edge"
        value.0 = 1
        value.1 = 2
        insert(FN_ARG[0].1, value)
        goto the.end
        the.end:
      }
      |]

  it "generates code for a single non-recursive rule" $ do
    cg "single_nonrecursive_rule" `resultsIn` [text|
      |]

  it "generates nested searches correctly" $ do
    cg "multiple_rule_clauses" `resultsIn` [text|
      |]

  it "generates code for a rule with 2 clauses of same name" $ do
    cg "multiple_clauses_same_name" `resultsIn` [text|
      |]

  it "generates code for a rule where columns need to equal each other" $
    pending -- TODO use fixture: rule_equal_columns

  it "generates code for a single recursive rule" $ do
    cg "single_recursive_rule" `resultsIn` [text|
      |]

  -- TODO variant where one is recursive
  it "generates code for mutually recursive rules" $ do
    cg "mutually_recursive_rules" `resultsIn` [text|
      |]

  -- TODO tests for rules with >2 clauses, ...
