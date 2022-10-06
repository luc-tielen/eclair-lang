{-# LANGUAGE TypeFamilies, RankNTypes, QuasiQuotes #-}

module Test.Eclair.RA.LowerSpec
  ( module Test.Eclair.RA.LowerSpec
  ) where

import qualified Data.Text as T
import qualified Eclair.EIR.IR as EIR
import Eclair.AST.Analysis
import Eclair.Pretty
import Eclair
import System.FilePath
import Test.Hspec
import NeatInterpolation
import Control.Exception


cg :: FilePath -> IO T.Text
cg path = do
  let file = "tests/fixtures" </> path <.> "dl"
  eir <- compileEIR file
  pure $ printDoc eir

extractDeclTypeSnippet :: Text -> Text
extractDeclTypeSnippet result = extractedSnippet
  where
    extractedSnippet = T.strip $ unlines $ reAddHeader $ map dedent $ extract endLineNr
    extract end = take (endLineNr - 3) $ drop 3 ls
    reAddHeader body = ["declare_type Program", "{"] ++ body ++ ["}"]
    dedent = T.drop 2
    ls = lines result
    lineCount = length ls
    endTypeLineNr = find ((T.isPrefixOf "}" . T.stripStart) . snd) $ zip [0..] ls
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
      let fnBody = map dedent $ stripFnHeader begin end $ lines result
       in T.strip $ unlines $ reAddFnHeader fnBody
    -- 2 and 3 is to strip fn ... + {}
    stripFnHeader begin end = take (end - begin - 3) . drop (begin + 2)
    reAddFnHeader body = ["fn " <> fnSignature, "{"] ++ body ++ ["}"]
    dedent = T.drop 2
    ls = zip [0..] $ lines result
    lineCount = length ls
    relevantLines = filter (T.isPrefixOf "fn" . T.stripStart . snd) ls
    fnName = viaNonEmpty head . drop 1 . words
    parsedLines = map (map fnName) relevantLines
    endLineNrs = drop 1 (map fst parsedLines) ++ [lineCount - 1]
    regions = zipWith (\(start, fn) end -> Region fn start end) parsedLines endLineNrs
    matchingRegion = (regionBegin &&& regionEnd) <$> find ((== fnName ("fn " <> fnSignature)) . regionName) regions

spec :: Spec
spec = describe "EIR Code Generation" $ parallel $ do
  it "generates empty run function for empty program" $ do
    let shouldFailWithCause m f = do
          try m >>= \case
            Left (SemanticErr _ _ errs) ->
              f errs `shouldNotBe` []
            result ->
              panic $ "Expected a failure, but got: " <> show result
    cg "empty" `shouldFailWithCause` emptyModules

  it "generates code for a single fact" $ do
    eir <- cg "single_fact"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        symbol_table
        another btree(num_columns=3, index=[0,1,2], block_size=256, search_type=linear)
        edge btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        symbol_table.init(program.0)
        another.init_empty(program.1)
        edge.init_empty(program.2)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        symbol_table.destroy(FN_ARG[0].0)
        another.destroy(FN_ARG[0].1)
        edge.destroy(FN_ARG[0].2)
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
        another.insert(FN_ARG[0].1, value)
        value_1 = edge.stack_allocate Value
        value_1.0 = 2
        value_1.1 = 3
        edge.insert(FN_ARG[0].2, value_1)
        value_2 = edge.stack_allocate Value
        value_2.0 = 1
        value_2.1 = 2
        edge.insert(FN_ARG[0].2, value_2)
      }
      |]

  it "generates code for a single non-recursive rule" $ do
    eir <- cg "single_nonrecursive_rule"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        symbol_table
        edge btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
        path btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        symbol_table.init(program.0)
        edge.init_empty(program.1)
        path.init_empty(program.2)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        symbol_table.destroy(FN_ARG[0].0)
        edge.destroy(FN_ARG[0].1)
        path.destroy(FN_ARG[0].2)
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
        value_1 = edge.stack_allocate Value
        value_1.0 = 0
        value_1.1 = 0
        value_2 = edge.stack_allocate Value
        value_2.0 = 4294967295
        value_2.1 = 4294967295
        begin_iter = edge.stack_allocate Iter
        end_iter = edge.stack_allocate Iter
        edge.iter_lower_bound(FN_ARG[0].1, value_1, begin_iter)
        edge.iter_upper_bound(FN_ARG[0].1, value_2, end_iter)
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
          path.insert(FN_ARG[0].2, value_3)
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
        symbol_table
        first btree(num_columns=1, index=[0], block_size=256, search_type=linear)
        second btree(num_columns=2, index=[1,0], block_size=256, search_type=linear)
        third btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        symbol_table.init(program.0)
        first.init_empty(program.1)
        second.init_empty(program.2)
        third.init_empty(program.3)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        symbol_table.destroy(FN_ARG[0].0)
        first.destroy(FN_ARG[0].1)
        second.destroy(FN_ARG[0].2)
        third.destroy(FN_ARG[0].3)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = second.stack_allocate Value
        value.0 = 2
        value.1 = 3
        second.insert(FN_ARG[0].2, value)
        value_1 = first.stack_allocate Value
        value_1.0 = 1
        first.insert(FN_ARG[0].1, value_1)
        value_2 = first.stack_allocate Value
        value_2.0 = 0
        value_3 = first.stack_allocate Value
        value_3.0 = 4294967295
        begin_iter = first.stack_allocate Iter
        end_iter = first.stack_allocate Iter
        first.iter_lower_bound(FN_ARG[0].1, value_2, begin_iter)
        first.iter_upper_bound(FN_ARG[0].1, value_3, end_iter)
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
          second.iter_lower_bound(FN_ARG[0].2, value_4, begin_iter_1)
          second.iter_upper_bound(FN_ARG[0].2, value_5, end_iter_1)
          loop
          {
            condition_1 = second.iter_is_equal(begin_iter_1, end_iter_1)
            if (condition_1)
            {
              goto range_query.end_1
            }
            current_1 = second.iter_current(begin_iter_1)
            value_6 = third.stack_allocate Value
            value_6.0 = current_1.0
            value_6.1 = current.0
            third.insert(FN_ARG[0].3, value_6)
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
        symbol_table
        chain btree(num_columns=3, index=[0,1,2], block_size=256, search_type=linear)
        link btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        symbol_table.init(program.0)
        chain.init_empty(program.1)
        link.init_empty(program.2)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        symbol_table.destroy(FN_ARG[0].0)
        chain.destroy(FN_ARG[0].1)
        link.destroy(FN_ARG[0].2)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = link.stack_allocate Value
        value.0 = 1
        value.1 = 2
        link.insert(FN_ARG[0].2, value)
        value_1 = link.stack_allocate Value
        value_1.0 = 0
        value_1.1 = 0
        value_2 = link.stack_allocate Value
        value_2.0 = 4294967295
        value_2.1 = 4294967295
        begin_iter = link.stack_allocate Iter
        end_iter = link.stack_allocate Iter
        link.iter_lower_bound(FN_ARG[0].2, value_1, begin_iter)
        link.iter_upper_bound(FN_ARG[0].2, value_2, end_iter)
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
          link.iter_lower_bound(FN_ARG[0].2, value_3, begin_iter_1)
          link.iter_upper_bound(FN_ARG[0].2, value_4, end_iter_1)
          loop
          {
            condition_1 = link.iter_is_equal(begin_iter_1, end_iter_1)
            if (condition_1)
            {
              goto range_query.end_1
            }
            current_1 = link.iter_current(begin_iter_1)
            value_5 = chain.stack_allocate Value
            value_5.0 = current.0
            value_5.1 = current.1
            value_5.2 = current_1.1
            chain.insert(FN_ARG[0].1, value_5)
            link.iter_next(begin_iter_1)
          }
          range_query.end_1:
          link.iter_next(begin_iter)
        }
        range_query.end:
      }
      |]

  it "generates code for a rule where columns need to equal each other" $ do
    eir <- cg "clause_with_same_vars"
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = c.stack_allocate Value
        value.0 = 0
        value.1 = 0
        value.2 = 42
        value.3 = 0
        value.4 = 0
        value_1 = c.stack_allocate Value
        value_1.0 = 4294967295
        value_1.1 = 4294967295
        value_1.2 = 42
        value_1.3 = 4294967295
        value_1.4 = 4294967295
        begin_iter = c.stack_allocate Iter
        end_iter = c.stack_allocate Iter
        c.iter_lower_bound(FN_ARG[0].3, value, begin_iter)
        c.iter_upper_bound(FN_ARG[0].3, value_1, end_iter)
        loop
        {
          condition = c.iter_is_equal(begin_iter, end_iter)
          if (condition)
          {
            goto range_query.end
          }
          current = c.iter_current(begin_iter)
          value_2 = other.stack_allocate Value
          value_2.0 = current.0
          value_3 = other.stack_allocate Value
          value_3.0 = current.0
          begin_iter_1 = other.stack_allocate Iter
          end_iter_1 = other.stack_allocate Iter
          other.iter_lower_bound(FN_ARG[0].4, value_2, begin_iter_1)
          other.iter_upper_bound(FN_ARG[0].4, value_3, end_iter_1)
          loop
          {
            condition_1 = other.iter_is_equal(begin_iter_1, end_iter_1)
            if (condition_1)
            {
              goto range_query.end_1
            }
            current_1 = other.iter_current(begin_iter_1)
            condition_2 = current.0 == current.4
            if (condition_2)
            {
              condition_3 = current.0 == current.1
              if (condition_3)
              {
                value_4 = a.stack_allocate Value
                value_4.0 = current.0
                a.insert(FN_ARG[0].1, value_4)
              }
            }
            other.iter_next(begin_iter_1)
          }
          range_query.end_1:
          c.iter_next(begin_iter)
        }
        range_query.end:
        value_5 = b.stack_allocate Value
        value_5.0 = 0
        value_5.1 = 0
        value_6 = b.stack_allocate Value
        value_6.0 = 4294967295
        value_6.1 = 4294967295
        begin_iter_2 = b.stack_allocate Iter
        end_iter_2 = b.stack_allocate Iter
        b.iter_lower_bound(FN_ARG[0].2, value_5, begin_iter_2)
        b.iter_upper_bound(FN_ARG[0].2, value_6, end_iter_2)
        loop
        {
          condition_4 = b.iter_is_equal(begin_iter_2, end_iter_2)
          if (condition_4)
          {
            goto range_query.end_2
          }
          current_2 = b.iter_current(begin_iter_2)
          value_7 = other.stack_allocate Value
          value_7.0 = current_2.0
          value_8 = other.stack_allocate Value
          value_8.0 = current_2.0
          begin_iter_3 = other.stack_allocate Iter
          end_iter_3 = other.stack_allocate Iter
          other.iter_lower_bound(FN_ARG[0].4, value_7, begin_iter_3)
          other.iter_upper_bound(FN_ARG[0].4, value_8, end_iter_3)
          loop
          {
            condition_5 = other.iter_is_equal(begin_iter_3, end_iter_3)
            if (condition_5)
            {
              goto range_query.end_3
            }
            current_3 = other.iter_current(begin_iter_3)
            condition_6 = current_2.0 == current_2.1
            if (condition_6)
            {
              value_9 = a.stack_allocate Value
              value_9.0 = current_2.0
              a.insert(FN_ARG[0].1, value_9)
            }
            other.iter_next(begin_iter_3)
          }
          range_query.end_3:
          b.iter_next(begin_iter_2)
        }
        range_query.end_2:
      }
      |]

  it "generates code for a rule containing an equality statement" $ do
    eir <- cg "assignment"
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        symbol_table
        fact1 btree(num_columns=2, index=[1], block_size=256, search_type=linear)
        fact2 btree(num_columns=2, index=[0,1], block_size=256, search_type=linear)
      }
      |]
    extractFnSnippet eir "eclair_program_init() -> *Program" `shouldBe` Just [text|
      fn eclair_program_init() -> *Program
      {
        program = heap_allocate_program
        symbol_table.init(program.0)
        fact1.init_empty(program.1)
        fact2.init_empty(program.2)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        symbol_table.destroy(FN_ARG[0].0)
        fact1.destroy(FN_ARG[0].1)
        fact2.destroy(FN_ARG[0].2)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = fact1.stack_allocate Value
        value.0 = 0
        value.1 = 0
        value_1 = fact1.stack_allocate Value
        value_1.0 = 4294967295
        value_1.1 = 4294967295
        begin_iter = fact1.stack_allocate Iter
        end_iter = fact1.stack_allocate Iter
        fact1.iter_lower_bound(FN_ARG[0].1, value, begin_iter)
        fact1.iter_upper_bound(FN_ARG[0].1, value_1, end_iter)
        loop
        {
          condition = fact1.iter_is_equal(begin_iter, end_iter)
          if (condition)
          {
            goto range_query.end
          }
          current = fact1.iter_current(begin_iter)
          condition_1 = 123 == current.1
          if (condition_1)
          {
            value_2 = fact2.stack_allocate Value
            value_2.0 = current.1
            value_2.1 = current.0
            fact2.insert(FN_ARG[0].2, value_2)
          }
          fact1.iter_next(begin_iter)
        }
        range_query.end:
        value_3 = fact1.stack_allocate Value
        value_3.0 = 0
        value_3.1 = 0
        value_4 = fact1.stack_allocate Value
        value_4.0 = 4294967295
        value_4.1 = 4294967295
        begin_iter_1 = fact1.stack_allocate Iter
        end_iter_1 = fact1.stack_allocate Iter
        fact1.iter_lower_bound(FN_ARG[0].1, value_3, begin_iter_1)
        fact1.iter_upper_bound(FN_ARG[0].1, value_4, end_iter_1)
        loop
        {
          condition_2 = fact1.iter_is_equal(begin_iter_1, end_iter_1)
          if (condition_2)
          {
            goto range_query.end_1
          }
          current_1 = fact1.iter_current(begin_iter_1)
          value_5 = fact1.stack_allocate Value
          value_5.0 = 0
          value_5.1 = current_1.0
          value_6 = fact1.stack_allocate Value
          value_6.0 = 4294967295
          value_6.1 = current_1.0
          begin_iter_2 = fact1.stack_allocate Iter
          end_iter_2 = fact1.stack_allocate Iter
          fact1.iter_lower_bound(FN_ARG[0].1, value_5, begin_iter_2)
          fact1.iter_upper_bound(FN_ARG[0].1, value_6, end_iter_2)
          loop
          {
            condition_3 = fact1.iter_is_equal(begin_iter_2, end_iter_2)
            if (condition_3)
            {
              goto range_query.end_2
            }
            current_2 = fact1.iter_current(begin_iter_2)
            condition_4 = current_1.1 == current_1.0
            if (condition_4)
            {
              condition_5 = current_2.0 == 123
              if (condition_5)
              {
                value_7 = fact2.stack_allocate Value
                value_7.0 = current_1.0
                value_7.1 = 1
                fact2.insert(FN_ARG[0].2, value_7)
              }
            }
            fact1.iter_next(begin_iter_2)
          }
          range_query.end_2:
          fact1.iter_next(begin_iter_1)
        }
        range_query.end_1:
      }
      |]

  it "generates code for a single recursive rule" $ do
    eir <- cg "single_recursive_rule"
    -- NOTE: program for now also contains delta_ and new_ relations,
    -- probably it's more efficient to move these to the stack (but left out of scope for now)
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        symbol_table
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
        symbol_table.init(program.0)
        delta_path.init_empty(program.1)
        edge.init_empty(program.2)
        new_path.init_empty(program.3)
        path.init_empty(program.4)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        symbol_table.destroy(FN_ARG[0].0)
        delta_path.destroy(FN_ARG[0].1)
        edge.destroy(FN_ARG[0].2)
        new_path.destroy(FN_ARG[0].3)
        path.destroy(FN_ARG[0].4)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = edge.stack_allocate Value
        value.0 = 1
        value.1 = 2
        edge.insert(FN_ARG[0].2, value)
        begin_iter = path.stack_allocate Iter
        end_iter = path.stack_allocate Iter
        path.iter_begin(FN_ARG[0].4, begin_iter)
        path.iter_end(FN_ARG[0].4, end_iter)
        path.insert_range(FN_ARG[0].1, begin_iter, end_iter)
        loop
        {
          new_path.purge(FN_ARG[0].3)
          value_1 = edge.stack_allocate Value
          value_1.0 = 0
          value_1.1 = 0
          value_2 = edge.stack_allocate Value
          value_2.0 = 4294967295
          value_2.1 = 4294967295
          begin_iter_1 = edge.stack_allocate Iter
          end_iter_1 = edge.stack_allocate Iter
          edge.iter_lower_bound(FN_ARG[0].2, value_1, begin_iter_1)
          edge.iter_upper_bound(FN_ARG[0].2, value_2, end_iter_1)
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
            delta_path.iter_lower_bound(FN_ARG[0].1, value_3, begin_iter_2)
            delta_path.iter_upper_bound(FN_ARG[0].1, value_4, end_iter_2)
            loop
            {
              condition_1 = delta_path.iter_is_equal(begin_iter_2, end_iter_2)
              if (condition_1)
              {
                goto range_query.end_1
              }
              current_1 = delta_path.iter_current(begin_iter_2)
              value_5 = path.stack_allocate Value
              value_5.0 = current.0
              value_5.1 = current_1.1
              contains_result = path.contains(FN_ARG[0].4, value_5)
              condition_2 = not contains_result
              if (condition_2)
              {
                value_6 = path.stack_allocate Value
                value_6.0 = current.0
                value_6.1 = current_1.1
                new_path.insert(FN_ARG[0].3, value_6)
              }
              delta_path.iter_next(begin_iter_2)
            }
            range_query.end_1:
            edge.iter_next(begin_iter_1)
          }
          range_query.end:
          condition_3 = new_path.is_empty(FN_ARG[0].3)
          if (condition_3)
          {
            goto loop.end
          }
          begin_iter_3 = path.stack_allocate Iter
          end_iter_3 = path.stack_allocate Iter
          new_path.iter_begin(FN_ARG[0].3, begin_iter_3)
          new_path.iter_end(FN_ARG[0].3, end_iter_3)
          new_path.insert_range(FN_ARG[0].4, begin_iter_3, end_iter_3)
          new_path.swap(FN_ARG[0].3, FN_ARG[0].1)
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
        symbol_table
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
        symbol_table.init(program.0)
        a.init_empty(program.1)
        b.init_empty(program.2)
        c.init_empty(program.3)
        d.init_empty(program.4)
        delta_b.init_empty(program.5)
        delta_c.init_empty(program.6)
        new_b.init_empty(program.7)
        new_c.init_empty(program.8)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        symbol_table.destroy(FN_ARG[0].0)
        a.destroy(FN_ARG[0].1)
        b.destroy(FN_ARG[0].2)
        c.destroy(FN_ARG[0].3)
        d.destroy(FN_ARG[0].4)
        delta_b.destroy(FN_ARG[0].5)
        delta_c.destroy(FN_ARG[0].6)
        new_b.destroy(FN_ARG[0].7)
        new_c.destroy(FN_ARG[0].8)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = d.stack_allocate Value
        value.0 = 3
        d.insert(FN_ARG[0].4, value)
        value_1 = c.stack_allocate Value
        value_1.0 = 2
        c.insert(FN_ARG[0].3, value_1)
        value_2 = b.stack_allocate Value
        value_2.0 = 1
        b.insert(FN_ARG[0].2, value_2)
        begin_iter = c.stack_allocate Iter
        end_iter = c.stack_allocate Iter
        c.iter_begin(FN_ARG[0].3, begin_iter)
        c.iter_end(FN_ARG[0].3, end_iter)
        c.insert_range(FN_ARG[0].6, begin_iter, end_iter)
        begin_iter_1 = b.stack_allocate Iter
        end_iter_1 = b.stack_allocate Iter
        b.iter_begin(FN_ARG[0].2, begin_iter_1)
        b.iter_end(FN_ARG[0].2, end_iter_1)
        b.insert_range(FN_ARG[0].5, begin_iter_1, end_iter_1)
        loop
        {
          new_c.purge(FN_ARG[0].8)
          new_b.purge(FN_ARG[0].7)
          parallel
          {
            value_3 = b.stack_allocate Value
            value_3.0 = 0
            value_4 = b.stack_allocate Value
            value_4.0 = 4294967295
            begin_iter_2 = b.stack_allocate Iter
            end_iter_2 = b.stack_allocate Iter
            b.iter_lower_bound(FN_ARG[0].2, value_3, begin_iter_2)
            b.iter_upper_bound(FN_ARG[0].2, value_4, end_iter_2)
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
              d.iter_lower_bound(FN_ARG[0].4, value_5, begin_iter_3)
              d.iter_upper_bound(FN_ARG[0].4, value_6, end_iter_3)
              loop
              {
                condition_1 = d.iter_is_equal(begin_iter_3, end_iter_3)
                if (condition_1)
                {
                  goto range_query.end_1
                }
                current_1 = d.iter_current(begin_iter_3)
                value_7 = c.stack_allocate Value
                value_7.0 = current.0
                contains_result = c.contains(FN_ARG[0].3, value_7)
                condition_2 = not contains_result
                if (condition_2)
                {
                  value_8 = c.stack_allocate Value
                  value_8.0 = current.0
                  new_c.insert(FN_ARG[0].8, value_8)
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
            c.iter_lower_bound(FN_ARG[0].3, value_9, begin_iter_4)
            c.iter_upper_bound(FN_ARG[0].3, value_10, end_iter_4)
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
              d.iter_lower_bound(FN_ARG[0].4, value_11, begin_iter_5)
              d.iter_upper_bound(FN_ARG[0].4, value_12, end_iter_5)
              loop
              {
                condition_4 = d.iter_is_equal(begin_iter_5, end_iter_5)
                if (condition_4)
                {
                  goto range_query.end_3
                }
                current_3 = d.iter_current(begin_iter_5)
                value_13 = b.stack_allocate Value
                value_13.0 = current_2.0
                contains_result_1 = b.contains(FN_ARG[0].2, value_13)
                condition_5 = not contains_result_1
                if (condition_5)
                {
                  value_14 = b.stack_allocate Value
                  value_14.0 = current_2.0
                  new_b.insert(FN_ARG[0].7, value_14)
                }
                d.iter_next(begin_iter_5)
              }
              range_query.end_3:
              c.iter_next(begin_iter_4)
            }
            range_query.end_2:
          }
          condition_6 = new_b.is_empty(FN_ARG[0].7)
          if (condition_6)
          {
            condition_7 = new_c.is_empty(FN_ARG[0].8)
            if (condition_7)
            {
              goto loop.end
            }
          }
          begin_iter_6 = c.stack_allocate Iter
          end_iter_6 = c.stack_allocate Iter
          new_c.iter_begin(FN_ARG[0].8, begin_iter_6)
          new_c.iter_end(FN_ARG[0].8, end_iter_6)
          new_c.insert_range(FN_ARG[0].3, begin_iter_6, end_iter_6)
          new_c.swap(FN_ARG[0].8, FN_ARG[0].6)
          begin_iter_7 = b.stack_allocate Iter
          end_iter_7 = b.stack_allocate Iter
          new_b.iter_begin(FN_ARG[0].7, begin_iter_7)
          new_b.iter_end(FN_ARG[0].7, end_iter_7)
          new_b.insert_range(FN_ARG[0].2, begin_iter_7, end_iter_7)
          new_b.swap(FN_ARG[0].7, FN_ARG[0].5)
        }
        loop.end:
        value_15 = b.stack_allocate Value
        value_15.0 = 0
        value_16 = b.stack_allocate Value
        value_16.0 = 4294967295
        begin_iter_8 = b.stack_allocate Iter
        end_iter_8 = b.stack_allocate Iter
        b.iter_lower_bound(FN_ARG[0].2, value_15, begin_iter_8)
        b.iter_upper_bound(FN_ARG[0].2, value_16, end_iter_8)
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
          c.iter_lower_bound(FN_ARG[0].3, value_17, begin_iter_9)
          c.iter_upper_bound(FN_ARG[0].3, value_18, end_iter_9)
          loop
          {
            condition_9 = c.iter_is_equal(begin_iter_9, end_iter_9)
            if (condition_9)
            {
              goto range_query.end_5
            }
            current_5 = c.iter_current(begin_iter_9)
            value_19 = a.stack_allocate Value
            value_19.0 = current_4.0
            a.insert(FN_ARG[0].1, value_19)
            c.iter_next(begin_iter_9)
          }
          range_query.end_5:
          b.iter_next(begin_iter_8)
        }
        range_query.end_4:
      }
      |]

  -- TODO tests for rules with >2 clauses, ...

  it "generates code for program without top level facts" $ do
    eir <- cg "no_top_level_facts"
    -- NOTE: program for now also contains delta_ and new_ relations,
    -- probably it's more efficient to move these to the stack (but left out of scope for now)
    extractDeclTypeSnippet eir `shouldBe` [text|
      declare_type Program
      {
        symbol_table
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
        symbol_table.init(program.0)
        delta_path.init_empty(program.1)
        edge.init_empty(program.2)
        new_path.init_empty(program.3)
        path.init_empty(program.4)
        return program
      }
      |]
    extractFnSnippet eir "eclair_program_destroy(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_destroy(*Program) -> Void
      {
        symbol_table.destroy(FN_ARG[0].0)
        delta_path.destroy(FN_ARG[0].1)
        edge.destroy(FN_ARG[0].2)
        new_path.destroy(FN_ARG[0].3)
        path.destroy(FN_ARG[0].4)
        free_program(FN_ARG[0])
      }
      |]
    extractFnSnippet eir "eclair_program_run(*Program) -> Void" `shouldBe` Just [text|
      fn eclair_program_run(*Program) -> Void
      {
        value = edge.stack_allocate Value
        value.0 = 0
        value.1 = 0
        value_1 = edge.stack_allocate Value
        value_1.0 = 4294967295
        value_1.1 = 4294967295
        begin_iter = edge.stack_allocate Iter
        end_iter = edge.stack_allocate Iter
        edge.iter_lower_bound(FN_ARG[0].2, value, begin_iter)
        edge.iter_upper_bound(FN_ARG[0].2, value_1, end_iter)
        loop
        {
          condition = edge.iter_is_equal(begin_iter, end_iter)
          if (condition)
          {
            goto range_query.end
          }
          current = edge.iter_current(begin_iter)
          value_2 = path.stack_allocate Value
          value_2.0 = current.0
          value_2.1 = current.1
          path.insert(FN_ARG[0].4, value_2)
          edge.iter_next(begin_iter)
        }
        range_query.end:
        begin_iter_1 = path.stack_allocate Iter
        end_iter_1 = path.stack_allocate Iter
        path.iter_begin(FN_ARG[0].4, begin_iter_1)
        path.iter_end(FN_ARG[0].4, end_iter_1)
        path.insert_range(FN_ARG[0].1, begin_iter_1, end_iter_1)
        loop
        {
          new_path.purge(FN_ARG[0].3)
          value_3 = edge.stack_allocate Value
          value_3.0 = 0
          value_3.1 = 0
          value_4 = edge.stack_allocate Value
          value_4.0 = 4294967295
          value_4.1 = 4294967295
          begin_iter_2 = edge.stack_allocate Iter
          end_iter_2 = edge.stack_allocate Iter
          edge.iter_lower_bound(FN_ARG[0].2, value_3, begin_iter_2)
          edge.iter_upper_bound(FN_ARG[0].2, value_4, end_iter_2)
          loop
          {
            condition_1 = edge.iter_is_equal(begin_iter_2, end_iter_2)
            if (condition_1)
            {
              goto range_query.end_1
            }
            current_1 = edge.iter_current(begin_iter_2)
            value_5 = path.stack_allocate Value
            value_5.0 = current_1.1
            value_5.1 = 0
            value_6 = path.stack_allocate Value
            value_6.0 = current_1.1
            value_6.1 = 4294967295
            begin_iter_3 = path.stack_allocate Iter
            end_iter_3 = path.stack_allocate Iter
            delta_path.iter_lower_bound(FN_ARG[0].1, value_5, begin_iter_3)
            delta_path.iter_upper_bound(FN_ARG[0].1, value_6, end_iter_3)
            loop
            {
              condition_2 = delta_path.iter_is_equal(begin_iter_3, end_iter_3)
              if (condition_2)
              {
                goto range_query.end_2
              }
              current_2 = delta_path.iter_current(begin_iter_3)
              value_7 = path.stack_allocate Value
              value_7.0 = current_1.0
              value_7.1 = current_2.1
              contains_result = path.contains(FN_ARG[0].4, value_7)
              condition_3 = not contains_result
              if (condition_3)
              {
                value_8 = path.stack_allocate Value
                value_8.0 = current_1.0
                value_8.1 = current_2.1
                new_path.insert(FN_ARG[0].3, value_8)
              }
              delta_path.iter_next(begin_iter_3)
            }
            range_query.end_2:
            edge.iter_next(begin_iter_2)
          }
          range_query.end_1:
          condition_4 = new_path.is_empty(FN_ARG[0].3)
          if (condition_4)
          {
            goto loop.end
          }
          begin_iter_4 = path.stack_allocate Iter
          end_iter_4 = path.stack_allocate Iter
          new_path.iter_begin(FN_ARG[0].3, begin_iter_4)
          new_path.iter_end(FN_ARG[0].3, end_iter_4)
          new_path.insert_range(FN_ARG[0].4, begin_iter_4, end_iter_4)
          new_path.swap(FN_ARG[0].3, FN_ARG[0].1)
        }
        loop.end:
      }
      |]

