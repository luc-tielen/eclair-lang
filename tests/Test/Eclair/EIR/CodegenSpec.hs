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
import Eclair.Pretty
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
    Right eir -> pure $ printDoc eir

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
spec = describe "EIR Code Generation" $ parallel $ do
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
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        init_empty(program.0)
        init_empty(program.1)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program)" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        destroy(FN_ARG[0].0)
        destroy(FN_ARG[0].1)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program)" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
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
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        init_empty(program.0)
        init_empty(program.1)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program)" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        destroy(FN_ARG[0].0)
        destroy(FN_ARG[0].1)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program)" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
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
          condition = iter_is_equal(begin_iter, end_iter)
          if (condition)
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
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        init_empty(program.0)
        init_empty(program.1)
        init_empty(program.2)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program)" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        destroy(FN_ARG[0].0)
        destroy(FN_ARG[0].1)
        destroy(FN_ARG[0].2)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program)" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> *Program
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
          condition = iter_is_equal(begin_iter, end_iter)
          if (condition)
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
            condition_1 = iter_is_equal(begin_iter_1, end_iter_1)
            if (condition_1)
            {
              goto range_query.end_1
            }
            current_1 = iter_current(begin_iter_1)
            condition_2 = current_1.1 == current.0
            if (condition_2)
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

  it "generates code for a rule with 2 clauses of same name" $ do
    eir <- cg "multiple_clauses_same_name"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        btree(num_columns=3, index=[0,1,2], block_size=256, search_type=linear)
        btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init()" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        init_empty(program.0)
        init_empty(program.1)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program)" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        destroy(FN_ARG[0].0)
        destroy(FN_ARG[0].1)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program)" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
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
          condition = iter_is_equal(begin_iter, end_iter)
          if (condition)
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
            condition_1 = iter_is_equal(begin_iter_1, end_iter_1)
            if (condition_1)
            {
              goto range_query.end_1
            }
            current_1 = iter_current(begin_iter_1)
            condition_2 = current_1.0 == current.1
            if (condition_2)
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

  it "generates code for a rule where columns need to equal each other" $ do
    pending -- TODO: cg "rule_equal_columns"

  it "generates code for a single recursive rule" $ do
    eir <- cg "single_recursive_rule"
    -- NOTE: program for now also contains delta_ and new_ relations,
    -- probably it's more efficient to move these to the stack (but left out of scope for now)
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
        btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
        btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
        btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init()" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        init_empty(program.0)
        init_empty(program.1)
        init_empty(program.2)
        init_empty(program.3)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program)" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        destroy(FN_ARG[0].0)
        destroy(FN_ARG[0].1)
        destroy(FN_ARG[0].2)
        destroy(FN_ARG[0].3)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program)" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = stack_allocate Value "edge"
        value.0 = 1
        value.1 = 2
        insert(FN_ARG[0].1, value)
        merge(FN_ARG[0].3, FN_ARG[0].0)
        loop
        {
          purge(FN_ARG[0].2)
          value_1 = stack_allocate Value "edge"
          value_1.0 = 0
          value_1.1 = 0
          value_2 = stack_allocate Value "edge"
          value_2.0 = 4294967295
          value_2.1 = 4294967295
          begin_iter = stack_allocate Iter "edge"
          end_iter = stack_allocate Iter "edge"
          iter_lower_bound(FN_ARG[0].1, value_1, begin_iter)
          iter_upper_bound(FN_ARG[0].1, value_2, end_iter)
          loop
          {
            condition = iter_is_equal(begin_iter, end_iter)
            if (condition)
            {
              goto range_query.end
            }
            current = iter_current(begin_iter)
            value_3 = stack_allocate Value "path"
            value_3.0 = current.1
            value_3.1 = 0
            value_4 = stack_allocate Value "path"
            value_4.0 = current.1
            value_4.1 = 4294967295
            begin_iter_1 = stack_allocate Iter "path"
            end_iter_1 = stack_allocate Iter "path"
            iter_lower_bound(FN_ARG[0].0, value_3, begin_iter_1)
            iter_upper_bound(FN_ARG[0].0, value_4, end_iter_1)
            loop
            {
              condition_1 = iter_is_equal(begin_iter_1, end_iter_1)
              if (condition_1)
              {
                goto range_query.end_1
              }
              current_1 = iter_current(begin_iter_1)
              bool = current_1.0 == current.1
              value_5 = stack_allocate Value "path"
              value_5.0 = current.0
              value_5.1 = current_1.1
              contains_result = contains(FN_ARG[0].3, value_5)
              bool_1 = not contains_result
              condition_2 = bool && bool_1
              if (condition_2)
              {
                value_6 = stack_allocate Value "path"
                value_6.0 = current.0
                value_6.1 = current_1.1
                insert(FN_ARG[0].2, value_6)
              }
              iter_next(begin_iter_1)
            }
            range_query.end_1:
            iter_next(begin_iter)
          }
          range_query.end:
          condition_3 = is_empty(FN_ARG[0].2)
          if (condition_3)
          {
            goto loop.end
          }
          merge(FN_ARG[0].2, FN_ARG[0].3)
          swap(FN_ARG[0].2, FN_ARG[0].0)
        }
        loop.end:
      }
      |]

  -- TODO variant where one is recursive
  it "generates code for mutually recursive rules" $ do
    eir <- cg "mutually_recursive_rules"
    -- NOTE: program for now also contains delta_ and new_ relations,
    -- probably it's more efficient to move these to the stack (but left out of scope for now)
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        btree(num_columns=1, index=[0], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init()" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        init_empty(program.0)
        init_empty(program.1)
        init_empty(program.2)
        init_empty(program.3)
        init_empty(program.4)
        init_empty(program.5)
        init_empty(program.6)
        init_empty(program.7)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program)" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        destroy(FN_ARG[0].0)
        destroy(FN_ARG[0].1)
        destroy(FN_ARG[0].2)
        destroy(FN_ARG[0].3)
        destroy(FN_ARG[0].4)
        destroy(FN_ARG[0].5)
        destroy(FN_ARG[0].6)
        destroy(FN_ARG[0].7)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program)" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = stack_allocate Value "d"
        value.0 = 3
        insert(FN_ARG[0].3, value)
        value_1 = stack_allocate Value "c"
        value_1.0 = 2
        insert(FN_ARG[0].2, value_1)
        value_2 = stack_allocate Value "b"
        value_2.0 = 1
        insert(FN_ARG[0].1, value_2)
        merge(FN_ARG[0].2, FN_ARG[0].5)
        merge(FN_ARG[0].1, FN_ARG[0].4)
        loop
        {
          purge(FN_ARG[0].7)
          purge(FN_ARG[0].6)
          parallel
          {
            value_3 = stack_allocate Value "b"
            value_3.0 = 0
            value_4 = stack_allocate Value "b"
            value_4.0 = 4294967295
            begin_iter = stack_allocate Iter "b"
            end_iter = stack_allocate Iter "b"
            iter_lower_bound(FN_ARG[0].1, value_3, begin_iter)
            iter_upper_bound(FN_ARG[0].1, value_4, end_iter)
            loop
            {
              condition = iter_is_equal(begin_iter, end_iter)
              if (condition)
              {
                goto range_query.end
              }
              current = iter_current(begin_iter)
              value_5 = stack_allocate Value "d"
              value_5.0 = current.0
              value_6 = stack_allocate Value "d"
              value_6.0 = current.0
              begin_iter_1 = stack_allocate Iter "d"
              end_iter_1 = stack_allocate Iter "d"
              iter_lower_bound(FN_ARG[0].3, value_5, begin_iter_1)
              iter_upper_bound(FN_ARG[0].3, value_6, end_iter_1)
              loop
              {
                condition_1 = iter_is_equal(begin_iter_1, end_iter_1)
                if (condition_1)
                {
                  goto range_query.end_1
                }
                current_1 = iter_current(begin_iter_1)
                bool = current_1.0 == current.0
                value_7 = stack_allocate Value "c"
                value_7.0 = current.0
                contains_result = contains(FN_ARG[0].2, value_7)
                bool_1 = not contains_result
                condition_2 = bool && bool_1
                if (condition_2)
                {
                  value_8 = stack_allocate Value "c"
                  value_8.0 = current.0
                  insert(FN_ARG[0].7, value_8)
                }
                iter_next(begin_iter_1)
              }
              range_query.end_1:
              iter_next(begin_iter)
            }
            range_query.end:
            value_9 = stack_allocate Value "c"
            value_9.0 = 0
            value_10 = stack_allocate Value "c"
            value_10.0 = 4294967295
            begin_iter_2 = stack_allocate Iter "c"
            end_iter_2 = stack_allocate Iter "c"
            iter_lower_bound(FN_ARG[0].2, value_9, begin_iter_2)
            iter_upper_bound(FN_ARG[0].2, value_10, end_iter_2)
            loop
            {
              condition_3 = iter_is_equal(begin_iter_2, end_iter_2)
              if (condition_3)
              {
                goto range_query.end_2
              }
              current_2 = iter_current(begin_iter_2)
              value_11 = stack_allocate Value "d"
              value_11.0 = current_2.0
              value_12 = stack_allocate Value "d"
              value_12.0 = current_2.0
              begin_iter_3 = stack_allocate Iter "d"
              end_iter_3 = stack_allocate Iter "d"
              iter_lower_bound(FN_ARG[0].3, value_11, begin_iter_3)
              iter_upper_bound(FN_ARG[0].3, value_12, end_iter_3)
              loop
              {
                condition_4 = iter_is_equal(begin_iter_3, end_iter_3)
                if (condition_4)
                {
                  goto range_query.end_3
                }
                current_3 = iter_current(begin_iter_3)
                bool_2 = current_3.0 == current_2.0
                value_13 = stack_allocate Value "b"
                value_13.0 = current_2.0
                contains_result_1 = contains(FN_ARG[0].1, value_13)
                bool_3 = not contains_result_1
                condition_5 = bool_2 && bool_3
                if (condition_5)
                {
                  value_14 = stack_allocate Value "b"
                  value_14.0 = current_2.0
                  insert(FN_ARG[0].6, value_14)
                }
                iter_next(begin_iter_3)
              }
              range_query.end_3:
              iter_next(begin_iter_2)
            }
            range_query.end_2:
          }
          condition_6 = is_empty(FN_ARG[0].6)
          if (condition_6)
          {
            condition_7 = is_empty(FN_ARG[0].7)
            if (condition_7)
            {
              goto loop.end
            }
          }
          merge(FN_ARG[0].7, FN_ARG[0].2)
          swap(FN_ARG[0].7, FN_ARG[0].5)
          merge(FN_ARG[0].6, FN_ARG[0].1)
          swap(FN_ARG[0].6, FN_ARG[0].4)
        }
        loop.end:
        value_15 = stack_allocate Value "b"
        value_15.0 = 0
        value_16 = stack_allocate Value "b"
        value_16.0 = 4294967295
        begin_iter_4 = stack_allocate Iter "b"
        end_iter_4 = stack_allocate Iter "b"
        iter_lower_bound(FN_ARG[0].1, value_15, begin_iter_4)
        iter_upper_bound(FN_ARG[0].1, value_16, end_iter_4)
        loop
        {
          condition_8 = iter_is_equal(begin_iter_4, end_iter_4)
          if (condition_8)
          {
            goto range_query.end_4
          }
          current_4 = iter_current(begin_iter_4)
          value_17 = stack_allocate Value "c"
          value_17.0 = current_4.0
          value_18 = stack_allocate Value "c"
          value_18.0 = current_4.0
          begin_iter_5 = stack_allocate Iter "c"
          end_iter_5 = stack_allocate Iter "c"
          iter_lower_bound(FN_ARG[0].2, value_17, begin_iter_5)
          iter_upper_bound(FN_ARG[0].2, value_18, end_iter_5)
          loop
          {
            condition_9 = iter_is_equal(begin_iter_5, end_iter_5)
            if (condition_9)
            {
              goto range_query.end_5
            }
            current_5 = iter_current(begin_iter_5)
            condition_10 = current_5.0 == current_4.0
            if (condition_10)
            {
              value_19 = stack_allocate Value "a"
              value_19.0 = current_4.0
              insert(FN_ARG[0].0, value_19)
            }
            iter_next(begin_iter_5)
          }
          range_query.end_5:
          iter_next(begin_iter_4)
        }
        range_query.end_4:
      }
      |]

  -- TODO tests for rules with >2 clauses, ...
