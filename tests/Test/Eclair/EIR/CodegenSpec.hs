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

extractDeclTypeSnippet :: Text -> Text
extractDeclTypeSnippet result = extractedSnippet
  where
    extractedSnippet = T.strip $ T.unlines $ reAddHeader $ map dedent $ extract endLineNr
    extract end = take (endLineNr - 3) $ drop 3 lines
    reAddHeader body = ["declare_type Program", "{"] ++ body ++ ["}"]
    dedent = T.drop 2
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
      let fnBody = map dedent $ stripFnHeader begin end $ T.lines result
       in T.strip $ T.unlines $ reAddFnHeader fnBody
    -- 2 and 3 is to strip fn ... + {}
    stripFnHeader begin end = take (end - begin - 3) . drop (begin + 2)
    reAddFnHeader body = ["fn " <> fnSignature, "{"] ++ body ++ ["}"]
    dedent = T.drop 2
    lines = zip [0..] $ T.lines result
    lineCount = length lines
    relevantLines = filter (T.isPrefixOf "fn" . T.stripStart . snd) lines
    parsedLines = map (map (head . drop 1 . T.words)) relevantLines
    endLineNrs = drop 1 (map fst parsedLines) ++ [lineCount - 1]
    regions = zipWith (\(start, fn) end -> Region fn start end) parsedLines endLineNrs
    matchingRegion = (regionBegin &&& regionEnd) <$> find ((== Just fnSignature) . regionName) regions

spec :: Spec
spec = fdescribe "EIR Code Generation" $ parallel $ do
  it "generates code for a single fact" $ do
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
        value_1 = stack_allocate Value "edge"
        value_1.0 = 2
        value_1.1 = 3
        insert(FN_ARG[0].1, value_1)
        value_2 = stack_allocate Value "edge"
        value_2.0 = 1
        value_2.1 = 2
        insert(FN_ARG[0].1, value_2)
      }
      |]

  it "generates code for a single non-recursive rule" $ do
    eir <- cg "single_nonrecursive_rule"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
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
        value = stack_allocate Value "edge"
        value.0 = 1
        value.1 = 2
        insert(FN_ARG[0].0, value)
        value_1 = stack_allocate Value "edge"
        value_1.0 = 0
        value_1.1 = 0
        value_2 = stack_allocate Value "edge"
        value_2.0 = 4294967295
        value_2.1 = 4294967295
        begin_iter = stack_allocate Iter "edge"
        end_iter = stack_allocate Iter "edge"
        iter_lower_bound(FN_ARG[0].0, value_1, begin_iter)
        iter_upper_bound(FN_ARG[0].0, value_2, end_iter)
        loop
        {
          if (iter_is_equal(begin_iter, end_iter))
          {
            goto range_query.end
          }
          current = iter_current(begin_iter)
          value_3 = stack_allocate Value "path"
          value_3.0 = current.0
          value_3.1 = current.1
          insert(FN_ARG[0].1, value_3)
          iter_next(begin_iter)
        }
        range_query.end:
      }
      |]

  it "generates nested searches correctly" $ do
    eir <- cg "multiple_rule_clauses"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        btree(num_columns=2, index=[1,0], block_size=256, search_type=linear)
        btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init()" `shouldBe` Just [text|
      fn eclair_program_init()
      {
        program = heap_allocate_program
        init_empty(program.0)
        init_empty(program.1)
        init_empty(program.2)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program)" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program)
      {
        destroy(FN_ARG[0].0)
        destroy(FN_ARG[0].1)
        destroy(FN_ARG[0].2)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program)" `shouldBe` Just [text|
      fn eclair_program_run(*Program)
      {
        value = stack_allocate Value "second"
        value.0 = 2
        value.1 = 3
        insert(FN_ARG[0].1, value)
        value_1 = stack_allocate Value "first"
        value_1.0 = 1
        insert(FN_ARG[0].0, value_1)
        value_2 = stack_allocate Value "first"
        value_2.0 = 0
        value_3 = stack_allocate Value "first"
        value_3.0 = 4294967295
        begin_iter = stack_allocate Iter "first"
        end_iter = stack_allocate Iter "first"
        iter_lower_bound(FN_ARG[0].0, value_2, begin_iter)
        iter_upper_bound(FN_ARG[0].0, value_3, end_iter)
        loop
        {
          if (iter_is_equal(begin_iter, end_iter))
          {
            goto range_query.end
          }
          current = iter_current(begin_iter)
          value_4 = stack_allocate Value "second"
          value_4.0 = 0
          value_4.1 = current.0
          value_5 = stack_allocate Value "second"
          value_5.0 = 4294967295
          value_5.1 = current.0
          begin_iter_1 = stack_allocate Iter "second"
          end_iter_1 = stack_allocate Iter "second"
          iter_lower_bound(FN_ARG[0].1, value_4, begin_iter_1)
          iter_upper_bound(FN_ARG[0].1, value_5, end_iter_1)
          loop
          {
            if (iter_is_equal(begin_iter_1, end_iter_1))
            {
              goto range_query.end_1
            }
            current_1 = iter_current(begin_iter_1)
            if (current_1.1 == current.0)
            {
              value_6 = stack_allocate Value "third"
              value_6.0 = current_1.0
              value_6.1 = current.0
              insert(FN_ARG[0].2, value_6)
            }
            iter_next(begin_iter_1)
          }
          range_query.end_1:
          iter_next(begin_iter)
        }
        range_query.end:
      }
      |]

  fit "generates code for a rule with 2 clauses of same name" $ do
    eir <- cg "multiple_clauses_same_name"
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
        value = stack_allocate Value "link"
        value.0 = 1
        value.1 = 2
        insert(FN_ARG[0].1, value)
        value_1 = stack_allocate Value "link"
        value_1.0 = 0
        value_1.1 = 0
        value_2 = stack_allocate Value "link"
        value_2.0 = 4294967295
        value_2.1 = 4294967295
        begin_iter = stack_allocate Iter "link"
        end_iter = stack_allocate Iter "link"
        iter_lower_bound(FN_ARG[0].1, value_1, begin_iter)
        iter_upper_bound(FN_ARG[0].1, value_2, end_iter)
        loop
        {
          if (iter_is_equal(begin_iter, end_iter))
          {
            goto range_query.end
          }
          current = iter_current(begin_iter)
          value_3 = stack_allocate Value "link"
          value_3.0 = current.1
          value_3.1 = 0
          value_4 = stack_allocate Value "link"
          value_4.0 = current.1
          value_4.1 = 4294967295
          begin_iter_1 = stack_allocate Iter "link"
          end_iter_1 = stack_allocate Iter "link"
          iter_lower_bound(FN_ARG[0].1, value_3, begin_iter_1)
          iter_upper_bound(FN_ARG[0].1, value_4, end_iter_1)
          loop
          {
            if (iter_is_equal(begin_iter_1, end_iter_1))
            {
              goto range_query.end_1
            }
            current_1 = iter_current(begin_iter_1)
            if (current_1.0 == current.1)
            {
              value_5 = stack_allocate Value "chain"
              value_5.0 = current.0
              value_5.1 = current.1
              value_5.2 = current_1.1
              insert(FN_ARG[0].0, value_5)
            }
            iter_next(begin_iter_1)
          }
          range_query.end_1:
          iter_next(begin_iter)
        }
        range_query.end:
      }
      |]

  {-
  it "generates code for a rule where columns need to equal each other" $
    pending -- TODO use fixture: rule_equal_columns

  it "generates code for a single recursive rule" $ do
    cg "single_recursive_rule" `resultsIn` [text|
      |]

  -- TODO variant where one is recursive
  it "generates code for mutually recursive rules" $ do
    cg "mutually_recursive_rules" `resultsIn` [text|
      |]
-}
  -- TODO tests for rules with >2 clauses, ...
