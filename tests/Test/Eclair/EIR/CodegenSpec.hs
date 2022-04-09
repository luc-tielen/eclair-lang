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
  result <- compileEIR file
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
    fnName = head . drop 1 . T.words
    parsedLines = map (map fnName) relevantLines
    endLineNrs = drop 1 (map fst parsedLines) ++ [lineCount - 1]
    regions = zipWith (\(start, fn) end -> Region fn start end) parsedLines endLineNrs
    matchingRegion = (regionBegin &&& regionEnd) <$> find ((== fnName ("fn " <> fnSignature)) . regionName) regions

spec :: Spec
spec = describe "EIR Code Generation" $ parallel $ do
  it "generates code for a single fact" $ do
    eir <- cg "single_fact"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        another btree(num_columns=3, index=[0,1,2], block_size=256, search_type=linear)
        edge btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        another.init_empty(program.0)
        edge.init_empty(program.1)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        another.destroy(FN_ARG[0].0)
        edge.destroy(FN_ARG[0].1)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = another.stack_allocate Value
        value.0 = 1
        value.1 = 2
        value.2 = 3
        another.insert(FN_ARG[0].0, value)
        value_1 = edge.stack_allocate Value
        value_1.0 = 2
        value_1.1 = 3
        edge.insert(FN_ARG[0].1, value_1)
        value_2 = edge.stack_allocate Value
        value_2.0 = 1
        value_2.1 = 2
        edge.insert(FN_ARG[0].1, value_2)
      }
      |]

  it "generates code for a single non-recursive rule" $ do
    eir <- cg "single_nonrecursive_rule"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        edge btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
        path btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        edge.init_empty(program.0)
        path.init_empty(program.1)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        edge.destroy(FN_ARG[0].0)
        path.destroy(FN_ARG[0].1)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = edge.stack_allocate Value
        value.0 = 1
        value.1 = 2
        edge.insert(FN_ARG[0].0, value)
        value_1 = edge.stack_allocate Value
        value_1.0 = 0
        value_1.1 = 0
        value_2 = edge.stack_allocate Value
        value_2.0 = 4294967295
        value_2.1 = 4294967295
        begin_iter = edge.stack_allocate Iter
        end_iter = edge.stack_allocate Iter
        edge.iter_lower_bound(FN_ARG[0].0, value_1, begin_iter)
        edge.iter_upper_bound(FN_ARG[0].0, value_2, end_iter)
        loop
        {
          condition = edge.iter_is_equal(begin_iter, end_iter)
          if (condition)
          {
            goto range_query.end
          }
          current = edge.iter_current(begin_iter)
          value_3 = path.stack_allocate Value
          value_3.0 = current.0
          value_3.1 = current.1
          path.insert(FN_ARG[0].1, value_3)
          edge.iter_next(begin_iter)
        }
        range_query.end:
      }
      |]

  it "generates nested searches correctly" $ do
    eir <- cg "multiple_rule_clauses"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        first btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        second btree(num_columns=2, index=[1,0], block_size=256, search_type=linear)
        third btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        first.init_empty(program.0)
        second.init_empty(program.1)
        third.init_empty(program.2)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        first.destroy(FN_ARG[0].0)
        second.destroy(FN_ARG[0].1)
        third.destroy(FN_ARG[0].2)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = second.stack_allocate Value
        value.0 = 2
        value.1 = 3
        second.insert(FN_ARG[0].1, value)
        value_1 = first.stack_allocate Value
        value_1.0 = 1
        first.insert(FN_ARG[0].0, value_1)
        value_2 = first.stack_allocate Value
        value_2.0 = 0
        value_3 = first.stack_allocate Value
        value_3.0 = 4294967295
        begin_iter = first.stack_allocate Iter
        end_iter = first.stack_allocate Iter
        first.iter_lower_bound(FN_ARG[0].0, value_2, begin_iter)
        first.iter_upper_bound(FN_ARG[0].0, value_3, end_iter)
        loop
        {
          condition = first.iter_is_equal(begin_iter, end_iter)
          if (condition)
          {
            goto range_query.end
          }
          current = first.iter_current(begin_iter)
          value_4 = second.stack_allocate Value
          value_4.0 = 0
          value_4.1 = current.0
          value_5 = second.stack_allocate Value
          value_5.0 = 4294967295
          value_5.1 = current.0
          begin_iter_1 = second.stack_allocate Iter
          end_iter_1 = second.stack_allocate Iter
          second.iter_lower_bound(FN_ARG[0].1, value_4, begin_iter_1)
          second.iter_upper_bound(FN_ARG[0].1, value_5, end_iter_1)
          loop
          {
            condition_1 = second.iter_is_equal(begin_iter_1, end_iter_1)
            if (condition_1)
            {
              goto range_query.end_1
            }
            current_1 = second.iter_current(begin_iter_1)
            condition_2 = current_1.1 == current.0
            if (condition_2)
            {
              value_6 = third.stack_allocate Value
              value_6.0 = current_1.0
              value_6.1 = current.0
              third.insert(FN_ARG[0].2, value_6)
            }
            second.iter_next(begin_iter_1)
          }
          range_query.end_1:
          first.iter_next(begin_iter)
        }
        range_query.end:
      }
      |]

  it "generates code for a rule with 2 clauses of same name" $ do
    eir <- cg "multiple_clauses_same_name"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        chain btree(num_columns=3, index=[0,1,2], block_size=256, search_type=linear)
        link btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        chain.init_empty(program.0)
        link.init_empty(program.1)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        chain.destroy(FN_ARG[0].0)
        link.destroy(FN_ARG[0].1)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = link.stack_allocate Value
        value.0 = 1
        value.1 = 2
        link.insert(FN_ARG[0].1, value)
        value_1 = link.stack_allocate Value
        value_1.0 = 0
        value_1.1 = 0
        value_2 = link.stack_allocate Value
        value_2.0 = 4294967295
        value_2.1 = 4294967295
        begin_iter = link.stack_allocate Iter
        end_iter = link.stack_allocate Iter
        link.iter_lower_bound(FN_ARG[0].1, value_1, begin_iter)
        link.iter_upper_bound(FN_ARG[0].1, value_2, end_iter)
        loop
        {
          condition = link.iter_is_equal(begin_iter, end_iter)
          if (condition)
          {
            goto range_query.end
          }
          current = link.iter_current(begin_iter)
          value_3 = link.stack_allocate Value
          value_3.0 = current.1
          value_3.1 = 0
          value_4 = link.stack_allocate Value
          value_4.0 = current.1
          value_4.1 = 4294967295
          begin_iter_1 = link.stack_allocate Iter
          end_iter_1 = link.stack_allocate Iter
          link.iter_lower_bound(FN_ARG[0].1, value_3, begin_iter_1)
          link.iter_upper_bound(FN_ARG[0].1, value_4, end_iter_1)
          loop
          {
            condition_1 = link.iter_is_equal(begin_iter_1, end_iter_1)
            if (condition_1)
            {
              goto range_query.end_1
            }
            current_1 = link.iter_current(begin_iter_1)
            condition_2 = current_1.0 == current.1
            if (condition_2)
            {
              value_5 = chain.stack_allocate Value
              value_5.0 = current.0
              value_5.1 = current.1
              value_5.2 = current_1.1
              chain.insert(FN_ARG[0].0, value_5)
            }
            link.iter_next(begin_iter_1)
          }
          range_query.end_1:
          link.iter_next(begin_iter)
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
        delta_path btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
        edge btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
        new_path btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
        path btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        delta_path.init_empty(program.0)
        edge.init_empty(program.1)
        new_path.init_empty(program.2)
        path.init_empty(program.3)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        delta_path.destroy(FN_ARG[0].0)
        edge.destroy(FN_ARG[0].1)
        new_path.destroy(FN_ARG[0].2)
        path.destroy(FN_ARG[0].3)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = edge.stack_allocate Value
        value.0 = 1
        value.1 = 2
        edge.insert(FN_ARG[0].1, value)
        begin_iter = path.stack_allocate Iter
        end_iter = path.stack_allocate Iter
        path.iter_begin(FN_ARG[0].3, begin_iter)
        path.iter_end(FN_ARG[0].3, end_iter)
        path.insert_range(FN_ARG[0].0, begin_iter, end_iter)
        loop
        {
          new_path.purge(FN_ARG[0].2)
          value_1 = edge.stack_allocate Value
          value_1.0 = 0
          value_1.1 = 0
          value_2 = edge.stack_allocate Value
          value_2.0 = 4294967295
          value_2.1 = 4294967295
          begin_iter_1 = edge.stack_allocate Iter
          end_iter_1 = edge.stack_allocate Iter
          edge.iter_lower_bound(FN_ARG[0].1, value_1, begin_iter_1)
          edge.iter_upper_bound(FN_ARG[0].1, value_2, end_iter_1)
          loop
          {
            condition = edge.iter_is_equal(begin_iter_1, end_iter_1)
            if (condition)
            {
              goto range_query.end
            }
            current = edge.iter_current(begin_iter_1)
            value_3 = path.stack_allocate Value
            value_3.0 = current.1
            value_3.1 = 0
            value_4 = path.stack_allocate Value
            value_4.0 = current.1
            value_4.1 = 4294967295
            begin_iter_2 = path.stack_allocate Iter
            end_iter_2 = path.stack_allocate Iter
            delta_path.iter_lower_bound(FN_ARG[0].0, value_3, begin_iter_2)
            delta_path.iter_upper_bound(FN_ARG[0].0, value_4, end_iter_2)
            loop
            {
              condition_1 = delta_path.iter_is_equal(begin_iter_2, end_iter_2)
              if (condition_1)
              {
                goto range_query.end_1
              }
              current_1 = delta_path.iter_current(begin_iter_2)
              bool = current_1.0 == current.1
              value_5 = path.stack_allocate Value
              value_5.0 = current.0
              value_5.1 = current_1.1
              contains_result = path.contains(FN_ARG[0].3, value_5)
              bool_1 = not contains_result
              condition_2 = bool && bool_1
              if (condition_2)
              {
                value_6 = path.stack_allocate Value
                value_6.0 = current.0
                value_6.1 = current_1.1
                new_path.insert(FN_ARG[0].2, value_6)
              }
              delta_path.iter_next(begin_iter_2)
            }
            range_query.end_1:
            edge.iter_next(begin_iter_1)
          }
          range_query.end:
          condition_3 = new_path.is_empty(FN_ARG[0].2)
          if (condition_3)
          {
            goto loop.end
          }
          begin_iter_3 = path.stack_allocate Iter
          end_iter_3 = path.stack_allocate Iter
          new_path.iter_begin(FN_ARG[0].2, begin_iter_3)
          new_path.iter_end(FN_ARG[0].2, end_iter_3)
          new_path.insert_range(FN_ARG[0].3, begin_iter_3, end_iter_3)
          new_path.swap(FN_ARG[0].2, FN_ARG[0].0)
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
        a btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        b btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        c btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        d btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        delta_b btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        delta_c btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        new_b btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        new_c btree(num_columns=1, index=[0], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        a.init_empty(program.0)
        b.init_empty(program.1)
        c.init_empty(program.2)
        d.init_empty(program.3)
        delta_b.init_empty(program.4)
        delta_c.init_empty(program.5)
        new_b.init_empty(program.6)
        new_c.init_empty(program.7)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        a.destroy(FN_ARG[0].0)
        b.destroy(FN_ARG[0].1)
        c.destroy(FN_ARG[0].2)
        d.destroy(FN_ARG[0].3)
        delta_b.destroy(FN_ARG[0].4)
        delta_c.destroy(FN_ARG[0].5)
        new_b.destroy(FN_ARG[0].6)
        new_c.destroy(FN_ARG[0].7)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = d.stack_allocate Value
        value.0 = 3
        d.insert(FN_ARG[0].3, value)
        value_1 = c.stack_allocate Value
        value_1.0 = 2
        c.insert(FN_ARG[0].2, value_1)
        value_2 = b.stack_allocate Value
        value_2.0 = 1
        b.insert(FN_ARG[0].1, value_2)
        begin_iter = c.stack_allocate Iter
        end_iter = c.stack_allocate Iter
        c.iter_begin(FN_ARG[0].2, begin_iter)
        c.iter_end(FN_ARG[0].2, end_iter)
        c.insert_range(FN_ARG[0].5, begin_iter, end_iter)
        begin_iter_1 = b.stack_allocate Iter
        end_iter_1 = b.stack_allocate Iter
        b.iter_begin(FN_ARG[0].1, begin_iter_1)
        b.iter_end(FN_ARG[0].1, end_iter_1)
        b.insert_range(FN_ARG[0].4, begin_iter_1, end_iter_1)
        loop
        {
          new_c.purge(FN_ARG[0].7)
          new_b.purge(FN_ARG[0].6)
          parallel
          {
            value_3 = b.stack_allocate Value
            value_3.0 = 0
            value_4 = b.stack_allocate Value
            value_4.0 = 4294967295
            begin_iter_2 = b.stack_allocate Iter
            end_iter_2 = b.stack_allocate Iter
            b.iter_lower_bound(FN_ARG[0].1, value_3, begin_iter_2)
            b.iter_upper_bound(FN_ARG[0].1, value_4, end_iter_2)
            loop
            {
              condition = b.iter_is_equal(begin_iter_2, end_iter_2)
              if (condition)
              {
                goto range_query.end
              }
              current = b.iter_current(begin_iter_2)
              value_5 = d.stack_allocate Value
              value_5.0 = current.0
              value_6 = d.stack_allocate Value
              value_6.0 = current.0
              begin_iter_3 = d.stack_allocate Iter
              end_iter_3 = d.stack_allocate Iter
              d.iter_lower_bound(FN_ARG[0].3, value_5, begin_iter_3)
              d.iter_upper_bound(FN_ARG[0].3, value_6, end_iter_3)
              loop
              {
                condition_1 = d.iter_is_equal(begin_iter_3, end_iter_3)
                if (condition_1)
                {
                  goto range_query.end_1
                }
                current_1 = d.iter_current(begin_iter_3)
                bool = current_1.0 == current.0
                value_7 = c.stack_allocate Value
                value_7.0 = current.0
                contains_result = c.contains(FN_ARG[0].2, value_7)
                bool_1 = not contains_result
                condition_2 = bool && bool_1
                if (condition_2)
                {
                  value_8 = c.stack_allocate Value
                  value_8.0 = current.0
                  new_c.insert(FN_ARG[0].7, value_8)
                }
                d.iter_next(begin_iter_3)
              }
              range_query.end_1:
              b.iter_next(begin_iter_2)
            }
            range_query.end:
            value_9 = c.stack_allocate Value
            value_9.0 = 0
            value_10 = c.stack_allocate Value
            value_10.0 = 4294967295
            begin_iter_4 = c.stack_allocate Iter
            end_iter_4 = c.stack_allocate Iter
            c.iter_lower_bound(FN_ARG[0].2, value_9, begin_iter_4)
            c.iter_upper_bound(FN_ARG[0].2, value_10, end_iter_4)
            loop
            {
              condition_3 = c.iter_is_equal(begin_iter_4, end_iter_4)
              if (condition_3)
              {
                goto range_query.end_2
              }
              current_2 = c.iter_current(begin_iter_4)
              value_11 = d.stack_allocate Value
              value_11.0 = current_2.0
              value_12 = d.stack_allocate Value
              value_12.0 = current_2.0
              begin_iter_5 = d.stack_allocate Iter
              end_iter_5 = d.stack_allocate Iter
              d.iter_lower_bound(FN_ARG[0].3, value_11, begin_iter_5)
              d.iter_upper_bound(FN_ARG[0].3, value_12, end_iter_5)
              loop
              {
                condition_4 = d.iter_is_equal(begin_iter_5, end_iter_5)
                if (condition_4)
                {
                  goto range_query.end_3
                }
                current_3 = d.iter_current(begin_iter_5)
                bool_2 = current_3.0 == current_2.0
                value_13 = b.stack_allocate Value
                value_13.0 = current_2.0
                contains_result_1 = b.contains(FN_ARG[0].1, value_13)
                bool_3 = not contains_result_1
                condition_5 = bool_2 && bool_3
                if (condition_5)
                {
                  value_14 = b.stack_allocate Value
                  value_14.0 = current_2.0
                  new_b.insert(FN_ARG[0].6, value_14)
                }
                d.iter_next(begin_iter_5)
              }
              range_query.end_3:
              c.iter_next(begin_iter_4)
            }
            range_query.end_2:
          }
          condition_6 = new_b.is_empty(FN_ARG[0].6)
          if (condition_6)
          {
            condition_7 = new_c.is_empty(FN_ARG[0].7)
            if (condition_7)
            {
              goto loop.end
            }
          }
          begin_iter_6 = c.stack_allocate Iter
          end_iter_6 = c.stack_allocate Iter
          new_c.iter_begin(FN_ARG[0].7, begin_iter_6)
          new_c.iter_end(FN_ARG[0].7, end_iter_6)
          new_c.insert_range(FN_ARG[0].2, begin_iter_6, end_iter_6)
          new_c.swap(FN_ARG[0].7, FN_ARG[0].5)
          begin_iter_7 = b.stack_allocate Iter
          end_iter_7 = b.stack_allocate Iter
          new_b.iter_begin(FN_ARG[0].6, begin_iter_7)
          new_b.iter_end(FN_ARG[0].6, end_iter_7)
          new_b.insert_range(FN_ARG[0].1, begin_iter_7, end_iter_7)
          new_b.swap(FN_ARG[0].6, FN_ARG[0].4)
        }
        loop.end:
        value_15 = b.stack_allocate Value
        value_15.0 = 0
        value_16 = b.stack_allocate Value
        value_16.0 = 4294967295
        begin_iter_8 = b.stack_allocate Iter
        end_iter_8 = b.stack_allocate Iter
        b.iter_lower_bound(FN_ARG[0].1, value_15, begin_iter_8)
        b.iter_upper_bound(FN_ARG[0].1, value_16, end_iter_8)
        loop
        {
          condition_8 = b.iter_is_equal(begin_iter_8, end_iter_8)
          if (condition_8)
          {
            goto range_query.end_4
          }
          current_4 = b.iter_current(begin_iter_8)
          value_17 = c.stack_allocate Value
          value_17.0 = current_4.0
          value_18 = c.stack_allocate Value
          value_18.0 = current_4.0
          begin_iter_9 = c.stack_allocate Iter
          end_iter_9 = c.stack_allocate Iter
          c.iter_lower_bound(FN_ARG[0].2, value_17, begin_iter_9)
          c.iter_upper_bound(FN_ARG[0].2, value_18, end_iter_9)
          loop
          {
            condition_9 = c.iter_is_equal(begin_iter_9, end_iter_9)
            if (condition_9)
            {
              goto range_query.end_5
            }
            current_5 = c.iter_current(begin_iter_9)
            condition_10 = current_5.0 == current_4.0
            if (condition_10)
            {
              value_19 = a.stack_allocate Value
              value_19.0 = current_4.0
              a.insert(FN_ARG[0].0, value_19)
            }
            c.iter_next(begin_iter_9)
          }
          range_query.end_5:
          b.iter_next(begin_iter_8)
        }
        range_query.end_4:
      }
      |]

  -- TODO tests for rules with >2 clauses, ...
