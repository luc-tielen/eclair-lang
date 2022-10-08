{-# LANGUAGE TypeFamilies, RankNTypes, QuasiQuotes #-}

module Test.Eclair.EIR.LowerSpec
  ( module Test.Eclair.EIR.LowerSpec
  ) where

import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Eclair
import Eclair.Pretty
import Eclair.AST.Analysis
import System.FilePath
import Test.Hspec
import NeatInterpolation
import LLVM.Codegen
import Control.Exception

-- Tip: compare LLVM IR with EIR from tests that generate pretty-printed EIR


cg :: FilePath -> IO T.Text
cg path = do
  let file = "tests/fixtures" </> path <.> "dl"
  llvm <- compileLLVM file
  pure $ ppllvm llvm

shouldFailWithCause :: (Eq a, Show a) => IO T.Text -> (SemanticErrors -> [a]) -> IO ()
shouldFailWithCause m f =
  try m >>= \case
    Left (SemanticErr _ _ errs) ->
      f errs `shouldNotBe` []
    result ->
      panic $ "Expected a failure, but got: " <> show result

extractDeclTypeSnippet :: Text -> Text
extractDeclTypeSnippet result =
  fromJust $ find (T.isPrefixOf "%program =") $ lines result

extractFnSnippet :: Text -> Text -> Maybe Text
extractFnSnippet result fnSignature = do
  let ls = lines result
  startLine <- L.findIndex (T.isInfixOf (fnSignature <> "(")) ls
  pure $ T.strip $ unlines $ map T.stripEnd $ takeWhile (/= "") $ drop startLine ls

-- TODO add tests for caching mechanism (e.g. single_nonrecursive_rule test)

spec :: Spec
spec = describe "LLVM Code Generation" $ parallel $ do
  it "generates almost no code for an empty program" $ do
    cg "empty" `shouldFailWithCause` emptyModules

  it "generates code for a single fact" $ do
    llvmIR <- cg "single_fact"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%symbol_table, %btree_t_0, %btree_t_1}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc %program* @eclair_program_init() {
      start:
        %memory_0 = call ccc i8* @malloc(i32 1592)
        %program_0 = bitcast i8* %memory_0 to %program*
        %0 = getelementptr %program, %program* %program_0, i32 0, i32 0
        call ccc void @symbol_table_init(%symbol_table* %0)
        %1 = getelementptr %program, %program* %program_0, i32 0, i32 1
        call ccc void @btree_init_empty_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %program_0, i32 0, i32 2
        call ccc void @btree_init_empty_1(%btree_t_1* %2)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc void @eclair_program_destroy(%program* %arg_0) {
      start:
        %0 = getelementptr %program, %program* %arg_0, i32 0, i32 0
        call ccc void @symbol_table_destroy(%symbol_table* %0)
        %1 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_destroy_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_destroy_1(%btree_t_1* %2)
        %memory_0 = bitcast %program* %arg_0 to i8*
        call ccc void @free(i8* %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc void @eclair_program_run(%program* %arg_0) {
      start:
        %value_0 = alloca [3 x i32], i32 1
        %0 = getelementptr [3 x i32], [3 x i32]* %value_0, i32 0, i32 0
        store i32 1, i32* %0
        %1 = getelementptr [3 x i32], [3 x i32]* %value_0, i32 0, i32 1
        store i32 2, i32* %1
        %2 = getelementptr [3 x i32], [3 x i32]* %value_0, i32 0, i32 2
        store i32 3, i32* %2
        %3 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        %4 = call ccc i1 @btree_insert_value_0(%btree_t_0* %3, [3 x i32]* %value_0)
        %value_1_0 = alloca [2 x i32], i32 1
        %5 = getelementptr [2 x i32], [2 x i32]* %value_1_0, i32 0, i32 0
        store i32 2, i32* %5
        %6 = getelementptr [2 x i32], [2 x i32]* %value_1_0, i32 0, i32 1
        store i32 3, i32* %6
        %7 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        %8 = call ccc i1 @btree_insert_value_1(%btree_t_1* %7, [2 x i32]* %value_1_0)
        %value_2_0 = alloca [2 x i32], i32 1
        %9 = getelementptr [2 x i32], [2 x i32]* %value_2_0, i32 0, i32 0
        store i32 1, i32* %9
        %10 = getelementptr [2 x i32], [2 x i32]* %value_2_0, i32 0, i32 1
        store i32 2, i32* %10
        %11 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        %12 = call ccc i1 @btree_insert_value_1(%btree_t_1* %11, [2 x i32]* %value_2_0)
        ret void
      }
      |]

  it "generates code for a single non-recursive rule" $ do
    llvmIR <- cg "single_nonrecursive_rule"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%symbol_table, %btree_t_0, %btree_t_0}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc %program* @eclair_program_init() {
      start:
        %memory_0 = call ccc i8* @malloc(i32 1592)
        %program_0 = bitcast i8* %memory_0 to %program*
        %0 = getelementptr %program, %program* %program_0, i32 0, i32 0
        call ccc void @symbol_table_init(%symbol_table* %0)
        %1 = getelementptr %program, %program* %program_0, i32 0, i32 1
        call ccc void @btree_init_empty_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %program_0, i32 0, i32 2
        call ccc void @btree_init_empty_0(%btree_t_0* %2)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc void @eclair_program_destroy(%program* %arg_0) {
      start:
        %0 = getelementptr %program, %program* %arg_0, i32 0, i32 0
        call ccc void @symbol_table_destroy(%symbol_table* %0)
        %1 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_destroy_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_destroy_0(%btree_t_0* %2)
        %memory_0 = bitcast %program* %arg_0 to i8*
        call ccc void @free(i8* %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc void @eclair_program_run(%program* %arg_0) {
      start:
        %value_0 = alloca [2 x i32], i32 1
        %0 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0, i32 0
        store i32 1, i32* %0
        %1 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0, i32 1
        store i32 2, i32* %1
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        %3 = call ccc i1 @btree_insert_value_0(%btree_t_0* %2, [2 x i32]* %value_0)
        %value_1_0 = alloca [2 x i32], i32 1
        %4 = getelementptr [2 x i32], [2 x i32]* %value_1_0, i32 0, i32 0
        store i32 0, i32* %4
        %5 = getelementptr [2 x i32], [2 x i32]* %value_1_0, i32 0, i32 1
        store i32 0, i32* %5
        %value_2_0 = alloca [2 x i32], i32 1
        %6 = getelementptr [2 x i32], [2 x i32]* %value_2_0, i32 0, i32 0
        store i32 4294967295, i32* %6
        %7 = getelementptr [2 x i32], [2 x i32]* %value_2_0, i32 0, i32 1
        store i32 4294967295, i32* %7
        %begin_iter_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_0 = alloca %btree_iterator_t_0, i32 1
        %8 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_lower_bound_0(%btree_t_0* %8, [2 x i32]* %value_1_0, %btree_iterator_t_0* %begin_iter_0)
        %9 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_upper_bound_0(%btree_t_0* %9, [2 x i32]* %value_2_0, %btree_iterator_t_0* %end_iter_0)
        br label %loop_0
      loop_0:
        %condition_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_0, %btree_iterator_t_0* %end_iter_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 = call ccc [2 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_0)
        %value_3_0 = alloca [2 x i32], i32 1
        %10 = getelementptr [2 x i32], [2 x i32]* %value_3_0, i32 0, i32 0
        %11 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 0
        %12 = load i32, i32* %11
        store i32 %12, i32* %10
        %13 = getelementptr [2 x i32], [2 x i32]* %value_3_0, i32 0, i32 1
        %14 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 1
        %15 = load i32, i32* %14
        store i32 %15, i32* %13
        %16 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        %17 = call ccc i1 @btree_insert_value_0(%btree_t_0* %16, [2 x i32]* %value_3_0)
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_0)
        br label %loop_0
      range_query.end:
        ret void
      }
      |]

  it "generates nested searches correctly" $ do
    llvmIR <- cg "multiple_rule_clauses"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%symbol_table, %btree_t_0, %btree_t_1, %btree_t_2}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc %program* @eclair_program_init() {
      start:
        %memory_0 = call ccc i8* @malloc(i32 1608)
        %program_0 = bitcast i8* %memory_0 to %program*
        %0 = getelementptr %program, %program* %program_0, i32 0, i32 0
        call ccc void @symbol_table_init(%symbol_table* %0)
        %1 = getelementptr %program, %program* %program_0, i32 0, i32 1
        call ccc void @btree_init_empty_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %program_0, i32 0, i32 2
        call ccc void @btree_init_empty_1(%btree_t_1* %2)
        %3 = getelementptr %program, %program* %program_0, i32 0, i32 3
        call ccc void @btree_init_empty_2(%btree_t_2* %3)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc void @eclair_program_destroy(%program* %arg_0) {
      start:
        %0 = getelementptr %program, %program* %arg_0, i32 0, i32 0
        call ccc void @symbol_table_destroy(%symbol_table* %0)
        %1 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_destroy_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_destroy_1(%btree_t_1* %2)
        %3 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_destroy_2(%btree_t_2* %3)
        %memory_0 = bitcast %program* %arg_0 to i8*
        call ccc void @free(i8* %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc void @eclair_program_run(%program* %arg_0) {
      start:
        %value_0 = alloca [2 x i32], i32 1
        %0 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0, i32 0
        store i32 2, i32* %0
        %1 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0, i32 1
        store i32 3, i32* %1
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        %3 = call ccc i1 @btree_insert_value_1(%btree_t_1* %2, [2 x i32]* %value_0)
        %value_1_0 = alloca [1 x i32], i32 1
        %4 = getelementptr [1 x i32], [1 x i32]* %value_1_0, i32 0, i32 0
        store i32 1, i32* %4
        %5 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        %6 = call ccc i1 @btree_insert_value_0(%btree_t_0* %5, [1 x i32]* %value_1_0)
        %value_2_0 = alloca [1 x i32], i32 1
        %7 = getelementptr [1 x i32], [1 x i32]* %value_2_0, i32 0, i32 0
        store i32 0, i32* %7
        %value_3_0 = alloca [1 x i32], i32 1
        %8 = getelementptr [1 x i32], [1 x i32]* %value_3_0, i32 0, i32 0
        store i32 4294967295, i32* %8
        %begin_iter_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_0 = alloca %btree_iterator_t_0, i32 1
        %9 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_lower_bound_0(%btree_t_0* %9, [1 x i32]* %value_2_0, %btree_iterator_t_0* %begin_iter_0)
        %10 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_upper_bound_0(%btree_t_0* %10, [1 x i32]* %value_3_0, %btree_iterator_t_0* %end_iter_0)
        br label %loop_0
      loop_0:
        %condition_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_0, %btree_iterator_t_0* %end_iter_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 = call ccc [1 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_0)
        %value_4_0 = alloca [2 x i32], i32 1
        %11 = getelementptr [2 x i32], [2 x i32]* %value_4_0, i32 0, i32 0
        store i32 0, i32* %11
        %12 = getelementptr [2 x i32], [2 x i32]* %value_4_0, i32 0, i32 1
        %13 = getelementptr [1 x i32], [1 x i32]* %current_0, i32 0, i32 0
        %14 = load i32, i32* %13
        store i32 %14, i32* %12
        %value_5_0 = alloca [2 x i32], i32 1
        %15 = getelementptr [2 x i32], [2 x i32]* %value_5_0, i32 0, i32 0
        store i32 4294967295, i32* %15
        %16 = getelementptr [2 x i32], [2 x i32]* %value_5_0, i32 0, i32 1
        %17 = getelementptr [1 x i32], [1 x i32]* %current_0, i32 0, i32 0
        %18 = load i32, i32* %17
        store i32 %18, i32* %16
        %begin_iter_1_0 = alloca %btree_iterator_t_1, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_1, i32 1
        %19 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_lower_bound_1(%btree_t_1* %19, [2 x i32]* %value_4_0, %btree_iterator_t_1* %begin_iter_1_0)
        %20 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_upper_bound_1(%btree_t_1* %20, [2 x i32]* %value_5_0, %btree_iterator_t_1* %end_iter_1_0)
        br label %loop_1
      loop_1:
        %condition_1_0 = call ccc i1 @btree_iterator_is_equal_1(%btree_iterator_t_1* %begin_iter_1_0, %btree_iterator_t_1* %end_iter_1_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 = call ccc [2 x i32]* @btree_iterator_current_1(%btree_iterator_t_1* %begin_iter_1_0)
        %value_6_0 = alloca [2 x i32], i32 1
        %21 = getelementptr [2 x i32], [2 x i32]* %value_6_0, i32 0, i32 0
        %22 = getelementptr [2 x i32], [2 x i32]* %current_1_0, i32 0, i32 0
        %23 = load i32, i32* %22
        store i32 %23, i32* %21
        %24 = getelementptr [2 x i32], [2 x i32]* %value_6_0, i32 0, i32 1
        %25 = getelementptr [1 x i32], [1 x i32]* %current_0, i32 0, i32 0
        %26 = load i32, i32* %25
        store i32 %26, i32* %24
        %27 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        %28 = call ccc i1 @btree_insert_value_2(%btree_t_2* %27, [2 x i32]* %value_6_0)
        call ccc void @btree_iterator_next_1(%btree_iterator_t_1* %begin_iter_1_0)
        br label %loop_1
      range_query.end_1:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_0)
        br label %loop_0
      range_query.end:
        ret void
      }
      |]

  it "generates code for a rule with 2 clauses of same name" $ do
    llvmIR <- cg "multiple_clauses_same_name"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%symbol_table, %btree_t_0, %btree_t_1}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc %program* @eclair_program_init() {
      start:
        %memory_0 = call ccc i8* @malloc(i32 1592)
        %program_0 = bitcast i8* %memory_0 to %program*
        %0 = getelementptr %program, %program* %program_0, i32 0, i32 0
        call ccc void @symbol_table_init(%symbol_table* %0)
        %1 = getelementptr %program, %program* %program_0, i32 0, i32 1
        call ccc void @btree_init_empty_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %program_0, i32 0, i32 2
        call ccc void @btree_init_empty_1(%btree_t_1* %2)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc void @eclair_program_destroy(%program* %arg_0) {
      start:
        %0 = getelementptr %program, %program* %arg_0, i32 0, i32 0
        call ccc void @symbol_table_destroy(%symbol_table* %0)
        %1 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_destroy_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_destroy_1(%btree_t_1* %2)
        %memory_0 = bitcast %program* %arg_0 to i8*
        call ccc void @free(i8* %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc void @eclair_program_run(%program* %arg_0) {
      start:
        %value_0 = alloca [2 x i32], i32 1
        %0 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0, i32 0
        store i32 1, i32* %0
        %1 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0, i32 1
        store i32 2, i32* %1
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        %3 = call ccc i1 @btree_insert_value_1(%btree_t_1* %2, [2 x i32]* %value_0)
        %value_1_0 = alloca [2 x i32], i32 1
        %4 = getelementptr [2 x i32], [2 x i32]* %value_1_0, i32 0, i32 0
        store i32 0, i32* %4
        %5 = getelementptr [2 x i32], [2 x i32]* %value_1_0, i32 0, i32 1
        store i32 0, i32* %5
        %value_2_0 = alloca [2 x i32], i32 1
        %6 = getelementptr [2 x i32], [2 x i32]* %value_2_0, i32 0, i32 0
        store i32 4294967295, i32* %6
        %7 = getelementptr [2 x i32], [2 x i32]* %value_2_0, i32 0, i32 1
        store i32 4294967295, i32* %7
        %begin_iter_0 = alloca %btree_iterator_t_1, i32 1
        %end_iter_0 = alloca %btree_iterator_t_1, i32 1
        %8 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_lower_bound_1(%btree_t_1* %8, [2 x i32]* %value_1_0, %btree_iterator_t_1* %begin_iter_0)
        %9 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_upper_bound_1(%btree_t_1* %9, [2 x i32]* %value_2_0, %btree_iterator_t_1* %end_iter_0)
        br label %loop_0
      loop_0:
        %condition_0 = call ccc i1 @btree_iterator_is_equal_1(%btree_iterator_t_1* %begin_iter_0, %btree_iterator_t_1* %end_iter_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 = call ccc [2 x i32]* @btree_iterator_current_1(%btree_iterator_t_1* %begin_iter_0)
        %value_3_0 = alloca [2 x i32], i32 1
        %10 = getelementptr [2 x i32], [2 x i32]* %value_3_0, i32 0, i32 0
        %11 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 1
        %12 = load i32, i32* %11
        store i32 %12, i32* %10
        %13 = getelementptr [2 x i32], [2 x i32]* %value_3_0, i32 0, i32 1
        store i32 0, i32* %13
        %value_4_0 = alloca [2 x i32], i32 1
        %14 = getelementptr [2 x i32], [2 x i32]* %value_4_0, i32 0, i32 0
        %15 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 1
        %16 = load i32, i32* %15
        store i32 %16, i32* %14
        %17 = getelementptr [2 x i32], [2 x i32]* %value_4_0, i32 0, i32 1
        store i32 4294967295, i32* %17
        %begin_iter_1_0 = alloca %btree_iterator_t_1, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_1, i32 1
        %18 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_lower_bound_1(%btree_t_1* %18, [2 x i32]* %value_3_0, %btree_iterator_t_1* %begin_iter_1_0)
        %19 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_upper_bound_1(%btree_t_1* %19, [2 x i32]* %value_4_0, %btree_iterator_t_1* %end_iter_1_0)
        br label %loop_1
      loop_1:
        %condition_1_0 = call ccc i1 @btree_iterator_is_equal_1(%btree_iterator_t_1* %begin_iter_1_0, %btree_iterator_t_1* %end_iter_1_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 = call ccc [2 x i32]* @btree_iterator_current_1(%btree_iterator_t_1* %begin_iter_1_0)
        %value_5_0 = alloca [3 x i32], i32 1
        %20 = getelementptr [3 x i32], [3 x i32]* %value_5_0, i32 0, i32 0
        %21 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 0
        %22 = load i32, i32* %21
        store i32 %22, i32* %20
        %23 = getelementptr [3 x i32], [3 x i32]* %value_5_0, i32 0, i32 1
        %24 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 1
        %25 = load i32, i32* %24
        store i32 %25, i32* %23
        %26 = getelementptr [3 x i32], [3 x i32]* %value_5_0, i32 0, i32 2
        %27 = getelementptr [2 x i32], [2 x i32]* %current_1_0, i32 0, i32 1
        %28 = load i32, i32* %27
        store i32 %28, i32* %26
        %29 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        %30 = call ccc i1 @btree_insert_value_0(%btree_t_0* %29, [3 x i32]* %value_5_0)
        call ccc void @btree_iterator_next_1(%btree_iterator_t_1* %begin_iter_1_0)
        br label %loop_1
      range_query.end_1:
        call ccc void @btree_iterator_next_1(%btree_iterator_t_1* %begin_iter_0)
        br label %loop_0
      range_query.end:
        ret void
      }
      |]

  it "generates code for a rule where columns need to equal each other" $ do
    llvmIR <- cg "clause_with_same_vars"
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc void @eclair_program_run(%program* %arg_0) {
      start:
        %value_0 = alloca [5 x i32], i32 1
        %0 = getelementptr [5 x i32], [5 x i32]* %value_0, i32 0, i32 0
        store i32 0, i32* %0
        %1 = getelementptr [5 x i32], [5 x i32]* %value_0, i32 0, i32 1
        store i32 0, i32* %1
        %2 = getelementptr [5 x i32], [5 x i32]* %value_0, i32 0, i32 2
        store i32 42, i32* %2
        %3 = getelementptr [5 x i32], [5 x i32]* %value_0, i32 0, i32 3
        store i32 0, i32* %3
        %4 = getelementptr [5 x i32], [5 x i32]* %value_0, i32 0, i32 4
        store i32 0, i32* %4
        %value_1_0 = alloca [5 x i32], i32 1
        %5 = getelementptr [5 x i32], [5 x i32]* %value_1_0, i32 0, i32 0
        store i32 4294967295, i32* %5
        %6 = getelementptr [5 x i32], [5 x i32]* %value_1_0, i32 0, i32 1
        store i32 4294967295, i32* %6
        %7 = getelementptr [5 x i32], [5 x i32]* %value_1_0, i32 0, i32 2
        store i32 42, i32* %7
        %8 = getelementptr [5 x i32], [5 x i32]* %value_1_0, i32 0, i32 3
        store i32 4294967295, i32* %8
        %9 = getelementptr [5 x i32], [5 x i32]* %value_1_0, i32 0, i32 4
        store i32 4294967295, i32* %9
        %begin_iter_0 = alloca %btree_iterator_t_2, i32 1
        %end_iter_0 = alloca %btree_iterator_t_2, i32 1
        %10 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_lower_bound_2(%btree_t_2* %10, [5 x i32]* %value_0, %btree_iterator_t_2* %begin_iter_0)
        %11 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_upper_bound_2(%btree_t_2* %11, [5 x i32]* %value_1_0, %btree_iterator_t_2* %end_iter_0)
        br label %loop_0
      loop_0:
        %condition_0 = call ccc i1 @btree_iterator_is_equal_2(%btree_iterator_t_2* %begin_iter_0, %btree_iterator_t_2* %end_iter_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 = call ccc [5 x i32]* @btree_iterator_current_2(%btree_iterator_t_2* %begin_iter_0)
        %value_2_0 = alloca [1 x i32], i32 1
        %12 = getelementptr [1 x i32], [1 x i32]* %value_2_0, i32 0, i32 0
        %13 = getelementptr [5 x i32], [5 x i32]* %current_0, i32 0, i32 0
        %14 = load i32, i32* %13
        store i32 %14, i32* %12
        %value_3_0 = alloca [1 x i32], i32 1
        %15 = getelementptr [1 x i32], [1 x i32]* %value_3_0, i32 0, i32 0
        %16 = getelementptr [5 x i32], [5 x i32]* %current_0, i32 0, i32 0
        %17 = load i32, i32* %16
        store i32 %17, i32* %15
        %begin_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %18 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_lower_bound_0(%btree_t_0* %18, [1 x i32]* %value_2_0, %btree_iterator_t_0* %begin_iter_1_0)
        %19 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_upper_bound_0(%btree_t_0* %19, [1 x i32]* %value_3_0, %btree_iterator_t_0* %end_iter_1_0)
        br label %loop_1
      loop_1:
        %condition_1_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_1_0, %btree_iterator_t_0* %end_iter_1_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 = call ccc [1 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_1_0)
        %condition_2_0 = getelementptr [5 x i32], [5 x i32]* %current_0, i32 0, i32 0
        %condition_2_1 = load i32, i32* %condition_2_0
        %condition_2_2 = getelementptr [5 x i32], [5 x i32]* %current_0, i32 0, i32 4
        %condition_2_3 = load i32, i32* %condition_2_2
        %condition_2_4 = icmp eq i32 %condition_2_1, %condition_2_3
        br i1 %condition_2_4, label %if_2, label %end_if_3
      if_2:
        %condition_3_0 = getelementptr [5 x i32], [5 x i32]* %current_0, i32 0, i32 0
        %condition_3_1 = load i32, i32* %condition_3_0
        %condition_3_2 = getelementptr [5 x i32], [5 x i32]* %current_0, i32 0, i32 1
        %condition_3_3 = load i32, i32* %condition_3_2
        %condition_3_4 = icmp eq i32 %condition_3_1, %condition_3_3
        br i1 %condition_3_4, label %if_3, label %end_if_2
      if_3:
        %value_4_0 = alloca [1 x i32], i32 1
        %20 = getelementptr [1 x i32], [1 x i32]* %value_4_0, i32 0, i32 0
        %21 = getelementptr [5 x i32], [5 x i32]* %current_0, i32 0, i32 0
        %22 = load i32, i32* %21
        store i32 %22, i32* %20
        %23 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        %24 = call ccc i1 @btree_insert_value_0(%btree_t_0* %23, [1 x i32]* %value_4_0)
        br label %end_if_2
      end_if_2:
        br label %end_if_3
      end_if_3:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_1_0)
        br label %loop_1
      range_query.end_1:
        call ccc void @btree_iterator_next_2(%btree_iterator_t_2* %begin_iter_0)
        br label %loop_0
      range_query.end:
        %value_5_0 = alloca [2 x i32], i32 1
        %25 = getelementptr [2 x i32], [2 x i32]* %value_5_0, i32 0, i32 0
        store i32 0, i32* %25
        %26 = getelementptr [2 x i32], [2 x i32]* %value_5_0, i32 0, i32 1
        store i32 0, i32* %26
        %value_6_0 = alloca [2 x i32], i32 1
        %27 = getelementptr [2 x i32], [2 x i32]* %value_6_0, i32 0, i32 0
        store i32 4294967295, i32* %27
        %28 = getelementptr [2 x i32], [2 x i32]* %value_6_0, i32 0, i32 1
        store i32 4294967295, i32* %28
        %begin_iter_2_0 = alloca %btree_iterator_t_1, i32 1
        %end_iter_2_0 = alloca %btree_iterator_t_1, i32 1
        %29 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_lower_bound_1(%btree_t_1* %29, [2 x i32]* %value_5_0, %btree_iterator_t_1* %begin_iter_2_0)
        %30 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_upper_bound_1(%btree_t_1* %30, [2 x i32]* %value_6_0, %btree_iterator_t_1* %end_iter_2_0)
        br label %loop_2
      loop_2:
        %condition_4_0 = call ccc i1 @btree_iterator_is_equal_1(%btree_iterator_t_1* %begin_iter_2_0, %btree_iterator_t_1* %end_iter_2_0)
        br i1 %condition_4_0, label %if_4, label %end_if_4
      if_4:
        br label %range_query.end_2
      end_if_4:
        %current_2_0 = call ccc [2 x i32]* @btree_iterator_current_1(%btree_iterator_t_1* %begin_iter_2_0)
        %value_7_0 = alloca [1 x i32], i32 1
        %31 = getelementptr [1 x i32], [1 x i32]* %value_7_0, i32 0, i32 0
        %32 = getelementptr [2 x i32], [2 x i32]* %current_2_0, i32 0, i32 0
        %33 = load i32, i32* %32
        store i32 %33, i32* %31
        %value_8_0 = alloca [1 x i32], i32 1
        %34 = getelementptr [1 x i32], [1 x i32]* %value_8_0, i32 0, i32 0
        %35 = getelementptr [2 x i32], [2 x i32]* %current_2_0, i32 0, i32 0
        %36 = load i32, i32* %35
        store i32 %36, i32* %34
        %begin_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %37 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_lower_bound_0(%btree_t_0* %37, [1 x i32]* %value_7_0, %btree_iterator_t_0* %begin_iter_3_0)
        %38 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_upper_bound_0(%btree_t_0* %38, [1 x i32]* %value_8_0, %btree_iterator_t_0* %end_iter_3_0)
        br label %loop_3
      loop_3:
        %condition_5_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_3_0, %btree_iterator_t_0* %end_iter_3_0)
        br i1 %condition_5_0, label %if_5, label %end_if_5
      if_5:
        br label %range_query.end_3
      end_if_5:
        %current_3_0 = call ccc [1 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_3_0)
        %condition_6_0 = getelementptr [2 x i32], [2 x i32]* %current_2_0, i32 0, i32 0
        %condition_6_1 = load i32, i32* %condition_6_0
        %condition_6_2 = getelementptr [2 x i32], [2 x i32]* %current_2_0, i32 0, i32 1
        %condition_6_3 = load i32, i32* %condition_6_2
        %condition_6_4 = icmp eq i32 %condition_6_1, %condition_6_3
        br i1 %condition_6_4, label %if_6, label %end_if_6
      if_6:
        %value_9_0 = alloca [1 x i32], i32 1
        %39 = getelementptr [1 x i32], [1 x i32]* %value_9_0, i32 0, i32 0
        %40 = getelementptr [2 x i32], [2 x i32]* %current_2_0, i32 0, i32 0
        %41 = load i32, i32* %40
        store i32 %41, i32* %39
        %42 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        %43 = call ccc i1 @btree_insert_value_0(%btree_t_0* %42, [1 x i32]* %value_9_0)
        br label %end_if_6
      end_if_6:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_3_0)
        br label %loop_3
      range_query.end_3:
        call ccc void @btree_iterator_next_1(%btree_iterator_t_1* %begin_iter_2_0)
        br label %loop_2
      range_query.end_2:
        ret void
      }
      |]

  it "generates code for a single recursive rule" $ do
    llvmIR <- cg "single_recursive_rule"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%symbol_table, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc %program* @eclair_program_init() {
      start:
        %memory_0 = call ccc i8* @malloc(i32 1624)
        %program_0 = bitcast i8* %memory_0 to %program*
        %0 = getelementptr %program, %program* %program_0, i32 0, i32 0
        call ccc void @symbol_table_init(%symbol_table* %0)
        %1 = getelementptr %program, %program* %program_0, i32 0, i32 1
        call ccc void @btree_init_empty_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %program_0, i32 0, i32 2
        call ccc void @btree_init_empty_0(%btree_t_0* %2)
        %3 = getelementptr %program, %program* %program_0, i32 0, i32 3
        call ccc void @btree_init_empty_0(%btree_t_0* %3)
        %4 = getelementptr %program, %program* %program_0, i32 0, i32 4
        call ccc void @btree_init_empty_0(%btree_t_0* %4)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc void @eclair_program_destroy(%program* %arg_0) {
      start:
        %0 = getelementptr %program, %program* %arg_0, i32 0, i32 0
        call ccc void @symbol_table_destroy(%symbol_table* %0)
        %1 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_destroy_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_destroy_0(%btree_t_0* %2)
        %3 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_destroy_0(%btree_t_0* %3)
        %4 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_destroy_0(%btree_t_0* %4)
        %memory_0 = bitcast %program* %arg_0 to i8*
        call ccc void @free(i8* %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc void @eclair_program_run(%program* %arg_0) {
      start:
        %value_0 = alloca [2 x i32], i32 1
        %0 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0, i32 0
        store i32 1, i32* %0
        %1 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0, i32 1
        store i32 2, i32* %1
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        %3 = call ccc i1 @btree_insert_value_0(%btree_t_0* %2, [2 x i32]* %value_0)
        %begin_iter_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_0 = alloca %btree_iterator_t_0, i32 1
        %4 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_begin_0(%btree_t_0* %4, %btree_iterator_t_0* %begin_iter_0)
        %5 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_end_0(%btree_t_0* %5, %btree_iterator_t_0* %end_iter_0)
        %6 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_insert_range_delta_path_path(%btree_t_0* %6, %btree_iterator_t_0* %begin_iter_0, %btree_iterator_t_0* %end_iter_0)
        br label %loop_0
      loop_0:
        %7 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_clear_0(%btree_t_0* %7)
        %value_1_0 = alloca [2 x i32], i32 1
        %8 = getelementptr [2 x i32], [2 x i32]* %value_1_0, i32 0, i32 0
        store i32 0, i32* %8
        %9 = getelementptr [2 x i32], [2 x i32]* %value_1_0, i32 0, i32 1
        store i32 0, i32* %9
        %value_2_0 = alloca [2 x i32], i32 1
        %10 = getelementptr [2 x i32], [2 x i32]* %value_2_0, i32 0, i32 0
        store i32 4294967295, i32* %10
        %11 = getelementptr [2 x i32], [2 x i32]* %value_2_0, i32 0, i32 1
        store i32 4294967295, i32* %11
        %begin_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %12 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_lower_bound_0(%btree_t_0* %12, [2 x i32]* %value_1_0, %btree_iterator_t_0* %begin_iter_1_0)
        %13 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_upper_bound_0(%btree_t_0* %13, [2 x i32]* %value_2_0, %btree_iterator_t_0* %end_iter_1_0)
        br label %loop_1
      loop_1:
        %condition_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_1_0, %btree_iterator_t_0* %end_iter_1_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 = call ccc [2 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_1_0)
        %value_3_0 = alloca [2 x i32], i32 1
        %14 = getelementptr [2 x i32], [2 x i32]* %value_3_0, i32 0, i32 0
        %15 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 1
        %16 = load i32, i32* %15
        store i32 %16, i32* %14
        %17 = getelementptr [2 x i32], [2 x i32]* %value_3_0, i32 0, i32 1
        store i32 0, i32* %17
        %value_4_0 = alloca [2 x i32], i32 1
        %18 = getelementptr [2 x i32], [2 x i32]* %value_4_0, i32 0, i32 0
        %19 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 1
        %20 = load i32, i32* %19
        store i32 %20, i32* %18
        %21 = getelementptr [2 x i32], [2 x i32]* %value_4_0, i32 0, i32 1
        store i32 4294967295, i32* %21
        %begin_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %22 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_lower_bound_0(%btree_t_0* %22, [2 x i32]* %value_3_0, %btree_iterator_t_0* %begin_iter_2_0)
        %23 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_upper_bound_0(%btree_t_0* %23, [2 x i32]* %value_4_0, %btree_iterator_t_0* %end_iter_2_0)
        br label %loop_2
      loop_2:
        %condition_1_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_2_0, %btree_iterator_t_0* %end_iter_2_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 = call ccc [2 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_2_0)
        %value_5_0 = alloca [2 x i32], i32 1
        %24 = getelementptr [2 x i32], [2 x i32]* %value_5_0, i32 0, i32 0
        %25 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 0
        %26 = load i32, i32* %25
        store i32 %26, i32* %24
        %27 = getelementptr [2 x i32], [2 x i32]* %value_5_0, i32 0, i32 1
        %28 = getelementptr [2 x i32], [2 x i32]* %current_1_0, i32 0, i32 1
        %29 = load i32, i32* %28
        store i32 %29, i32* %27
        %contains_result_0 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        %contains_result_1 = call ccc i1 @btree_contains_0(%btree_t_0* %contains_result_0, [2 x i32]* %value_5_0)
        %condition_2_0 = select i1 %contains_result_1, i1 0, i1 1
        br i1 %condition_2_0, label %if_2, label %end_if_2
      if_2:
        %value_6_0 = alloca [2 x i32], i32 1
        %30 = getelementptr [2 x i32], [2 x i32]* %value_6_0, i32 0, i32 0
        %31 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 0
        %32 = load i32, i32* %31
        store i32 %32, i32* %30
        %33 = getelementptr [2 x i32], [2 x i32]* %value_6_0, i32 0, i32 1
        %34 = getelementptr [2 x i32], [2 x i32]* %current_1_0, i32 0, i32 1
        %35 = load i32, i32* %34
        store i32 %35, i32* %33
        %36 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        %37 = call ccc i1 @btree_insert_value_0(%btree_t_0* %36, [2 x i32]* %value_6_0)
        br label %end_if_2
      end_if_2:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_2_0)
        br label %loop_2
      range_query.end_1:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_1_0)
        br label %loop_1
      range_query.end:
        %condition_3_0 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        %condition_3_1 = call ccc i1 @btree_is_empty_0(%btree_t_0* %condition_3_0)
        br i1 %condition_3_1, label %if_3, label %end_if_3
      if_3:
        br label %loop.end
      end_if_3:
        %begin_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %38 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_begin_0(%btree_t_0* %38, %btree_iterator_t_0* %begin_iter_3_0)
        %39 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_end_0(%btree_t_0* %39, %btree_iterator_t_0* %end_iter_3_0)
        %40 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_insert_range_path_new_path(%btree_t_0* %40, %btree_iterator_t_0* %begin_iter_3_0, %btree_iterator_t_0* %end_iter_3_0)
        %41 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        %42 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_swap_0(%btree_t_0* %41, %btree_t_0* %42)
        br label %loop_0
      loop.end:
        ret void
      }
      |]

  -- TODO variant where one is recursive
  it "generates code for mutually recursive rules" $ do
    llvmIR <- cg "mutually_recursive_rules"
    extractDeclTypeSnippet llvmIR `shouldBe`
      "%program = type {%symbol_table, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc %program* @eclair_program_init() {
      start:
        %memory_0 = call ccc i8* @malloc(i32 1688)
        %program_0 = bitcast i8* %memory_0 to %program*
        %0 = getelementptr %program, %program* %program_0, i32 0, i32 0
        call ccc void @symbol_table_init(%symbol_table* %0)
        %1 = getelementptr %program, %program* %program_0, i32 0, i32 1
        call ccc void @btree_init_empty_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %program_0, i32 0, i32 2
        call ccc void @btree_init_empty_0(%btree_t_0* %2)
        %3 = getelementptr %program, %program* %program_0, i32 0, i32 3
        call ccc void @btree_init_empty_0(%btree_t_0* %3)
        %4 = getelementptr %program, %program* %program_0, i32 0, i32 4
        call ccc void @btree_init_empty_0(%btree_t_0* %4)
        %5 = getelementptr %program, %program* %program_0, i32 0, i32 5
        call ccc void @btree_init_empty_0(%btree_t_0* %5)
        %6 = getelementptr %program, %program* %program_0, i32 0, i32 6
        call ccc void @btree_init_empty_0(%btree_t_0* %6)
        %7 = getelementptr %program, %program* %program_0, i32 0, i32 7
        call ccc void @btree_init_empty_0(%btree_t_0* %7)
        %8 = getelementptr %program, %program* %program_0, i32 0, i32 8
        call ccc void @btree_init_empty_0(%btree_t_0* %8)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc void @eclair_program_destroy(%program* %arg_0) {
      start:
        %0 = getelementptr %program, %program* %arg_0, i32 0, i32 0
        call ccc void @symbol_table_destroy(%symbol_table* %0)
        %1 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_destroy_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_destroy_0(%btree_t_0* %2)
        %3 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_destroy_0(%btree_t_0* %3)
        %4 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_destroy_0(%btree_t_0* %4)
        %5 = getelementptr %program, %program* %arg_0, i32 0, i32 5
        call ccc void @btree_destroy_0(%btree_t_0* %5)
        %6 = getelementptr %program, %program* %arg_0, i32 0, i32 6
        call ccc void @btree_destroy_0(%btree_t_0* %6)
        %7 = getelementptr %program, %program* %arg_0, i32 0, i32 7
        call ccc void @btree_destroy_0(%btree_t_0* %7)
        %8 = getelementptr %program, %program* %arg_0, i32 0, i32 8
        call ccc void @btree_destroy_0(%btree_t_0* %8)
        %memory_0 = bitcast %program* %arg_0 to i8*
        call ccc void @free(i8* %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc void @eclair_program_run(%program* %arg_0) {
      start:
        %value_0 = alloca [1 x i32], i32 1
        %0 = getelementptr [1 x i32], [1 x i32]* %value_0, i32 0, i32 0
        store i32 3, i32* %0
        %1 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        %2 = call ccc i1 @btree_insert_value_0(%btree_t_0* %1, [1 x i32]* %value_0)
        %value_1_0 = alloca [1 x i32], i32 1
        %3 = getelementptr [1 x i32], [1 x i32]* %value_1_0, i32 0, i32 0
        store i32 2, i32* %3
        %4 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        %5 = call ccc i1 @btree_insert_value_0(%btree_t_0* %4, [1 x i32]* %value_1_0)
        %value_2_0 = alloca [1 x i32], i32 1
        %6 = getelementptr [1 x i32], [1 x i32]* %value_2_0, i32 0, i32 0
        store i32 1, i32* %6
        %7 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        %8 = call ccc i1 @btree_insert_value_0(%btree_t_0* %7, [1 x i32]* %value_2_0)
        %begin_iter_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_0 = alloca %btree_iterator_t_0, i32 1
        %9 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_begin_0(%btree_t_0* %9, %btree_iterator_t_0* %begin_iter_0)
        %10 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_end_0(%btree_t_0* %10, %btree_iterator_t_0* %end_iter_0)
        %11 = getelementptr %program, %program* %arg_0, i32 0, i32 6
        call ccc void @btree_insert_range_delta_c_c(%btree_t_0* %11, %btree_iterator_t_0* %begin_iter_0, %btree_iterator_t_0* %end_iter_0)
        %begin_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %12 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_begin_0(%btree_t_0* %12, %btree_iterator_t_0* %begin_iter_1_0)
        %13 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_end_0(%btree_t_0* %13, %btree_iterator_t_0* %end_iter_1_0)
        %14 = getelementptr %program, %program* %arg_0, i32 0, i32 5
        call ccc void @btree_insert_range_delta_b_b(%btree_t_0* %14, %btree_iterator_t_0* %begin_iter_1_0, %btree_iterator_t_0* %end_iter_1_0)
        br label %loop_0
      loop_0:
        %15 = getelementptr %program, %program* %arg_0, i32 0, i32 8
        call ccc void @btree_clear_0(%btree_t_0* %15)
        %16 = getelementptr %program, %program* %arg_0, i32 0, i32 7
        call ccc void @btree_clear_0(%btree_t_0* %16)
        %value_3_0 = alloca [1 x i32], i32 1
        %17 = getelementptr [1 x i32], [1 x i32]* %value_3_0, i32 0, i32 0
        store i32 0, i32* %17
        %value_4_0 = alloca [1 x i32], i32 1
        %18 = getelementptr [1 x i32], [1 x i32]* %value_4_0, i32 0, i32 0
        store i32 4294967295, i32* %18
        %begin_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %19 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_lower_bound_0(%btree_t_0* %19, [1 x i32]* %value_3_0, %btree_iterator_t_0* %begin_iter_2_0)
        %20 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_upper_bound_0(%btree_t_0* %20, [1 x i32]* %value_4_0, %btree_iterator_t_0* %end_iter_2_0)
        br label %loop_1
      loop_1:
        %condition_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_2_0, %btree_iterator_t_0* %end_iter_2_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 = call ccc [1 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_2_0)
        %value_5_0 = alloca [1 x i32], i32 1
        %21 = getelementptr [1 x i32], [1 x i32]* %value_5_0, i32 0, i32 0
        %22 = getelementptr [1 x i32], [1 x i32]* %current_0, i32 0, i32 0
        %23 = load i32, i32* %22
        store i32 %23, i32* %21
        %value_6_0 = alloca [1 x i32], i32 1
        %24 = getelementptr [1 x i32], [1 x i32]* %value_6_0, i32 0, i32 0
        %25 = getelementptr [1 x i32], [1 x i32]* %current_0, i32 0, i32 0
        %26 = load i32, i32* %25
        store i32 %26, i32* %24
        %begin_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %27 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_lower_bound_0(%btree_t_0* %27, [1 x i32]* %value_5_0, %btree_iterator_t_0* %begin_iter_3_0)
        %28 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_upper_bound_0(%btree_t_0* %28, [1 x i32]* %value_6_0, %btree_iterator_t_0* %end_iter_3_0)
        br label %loop_2
      loop_2:
        %condition_1_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_3_0, %btree_iterator_t_0* %end_iter_3_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 = call ccc [1 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_3_0)
        %value_7_0 = alloca [1 x i32], i32 1
        %29 = getelementptr [1 x i32], [1 x i32]* %value_7_0, i32 0, i32 0
        %30 = getelementptr [1 x i32], [1 x i32]* %current_0, i32 0, i32 0
        %31 = load i32, i32* %30
        store i32 %31, i32* %29
        %contains_result_0 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        %contains_result_1 = call ccc i1 @btree_contains_0(%btree_t_0* %contains_result_0, [1 x i32]* %value_7_0)
        %condition_2_0 = select i1 %contains_result_1, i1 0, i1 1
        br i1 %condition_2_0, label %if_2, label %end_if_2
      if_2:
        %value_8_0 = alloca [1 x i32], i32 1
        %32 = getelementptr [1 x i32], [1 x i32]* %value_8_0, i32 0, i32 0
        %33 = getelementptr [1 x i32], [1 x i32]* %current_0, i32 0, i32 0
        %34 = load i32, i32* %33
        store i32 %34, i32* %32
        %35 = getelementptr %program, %program* %arg_0, i32 0, i32 8
        %36 = call ccc i1 @btree_insert_value_0(%btree_t_0* %35, [1 x i32]* %value_8_0)
        br label %end_if_2
      end_if_2:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_3_0)
        br label %loop_2
      range_query.end_1:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_2_0)
        br label %loop_1
      range_query.end:
        %value_9_0 = alloca [1 x i32], i32 1
        %37 = getelementptr [1 x i32], [1 x i32]* %value_9_0, i32 0, i32 0
        store i32 0, i32* %37
        %value_10_0 = alloca [1 x i32], i32 1
        %38 = getelementptr [1 x i32], [1 x i32]* %value_10_0, i32 0, i32 0
        store i32 4294967295, i32* %38
        %begin_iter_4_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_4_0 = alloca %btree_iterator_t_0, i32 1
        %39 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_lower_bound_0(%btree_t_0* %39, [1 x i32]* %value_9_0, %btree_iterator_t_0* %begin_iter_4_0)
        %40 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_upper_bound_0(%btree_t_0* %40, [1 x i32]* %value_10_0, %btree_iterator_t_0* %end_iter_4_0)
        br label %loop_3
      loop_3:
        %condition_3_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_4_0, %btree_iterator_t_0* %end_iter_4_0)
        br i1 %condition_3_0, label %if_3, label %end_if_3
      if_3:
        br label %range_query.end_2
      end_if_3:
        %current_2_0 = call ccc [1 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_4_0)
        %value_11_0 = alloca [1 x i32], i32 1
        %41 = getelementptr [1 x i32], [1 x i32]* %value_11_0, i32 0, i32 0
        %42 = getelementptr [1 x i32], [1 x i32]* %current_2_0, i32 0, i32 0
        %43 = load i32, i32* %42
        store i32 %43, i32* %41
        %value_12_0 = alloca [1 x i32], i32 1
        %44 = getelementptr [1 x i32], [1 x i32]* %value_12_0, i32 0, i32 0
        %45 = getelementptr [1 x i32], [1 x i32]* %current_2_0, i32 0, i32 0
        %46 = load i32, i32* %45
        store i32 %46, i32* %44
        %begin_iter_5_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_5_0 = alloca %btree_iterator_t_0, i32 1
        %47 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_lower_bound_0(%btree_t_0* %47, [1 x i32]* %value_11_0, %btree_iterator_t_0* %begin_iter_5_0)
        %48 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_upper_bound_0(%btree_t_0* %48, [1 x i32]* %value_12_0, %btree_iterator_t_0* %end_iter_5_0)
        br label %loop_4
      loop_4:
        %condition_4_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_5_0, %btree_iterator_t_0* %end_iter_5_0)
        br i1 %condition_4_0, label %if_4, label %end_if_4
      if_4:
        br label %range_query.end_3
      end_if_4:
        %current_3_0 = call ccc [1 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_5_0)
        %value_13_0 = alloca [1 x i32], i32 1
        %49 = getelementptr [1 x i32], [1 x i32]* %value_13_0, i32 0, i32 0
        %50 = getelementptr [1 x i32], [1 x i32]* %current_2_0, i32 0, i32 0
        %51 = load i32, i32* %50
        store i32 %51, i32* %49
        %contains_result_1_0 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        %contains_result_1_1 = call ccc i1 @btree_contains_0(%btree_t_0* %contains_result_1_0, [1 x i32]* %value_13_0)
        %condition_5_0 = select i1 %contains_result_1_1, i1 0, i1 1
        br i1 %condition_5_0, label %if_5, label %end_if_5
      if_5:
        %value_14_0 = alloca [1 x i32], i32 1
        %52 = getelementptr [1 x i32], [1 x i32]* %value_14_0, i32 0, i32 0
        %53 = getelementptr [1 x i32], [1 x i32]* %current_2_0, i32 0, i32 0
        %54 = load i32, i32* %53
        store i32 %54, i32* %52
        %55 = getelementptr %program, %program* %arg_0, i32 0, i32 7
        %56 = call ccc i1 @btree_insert_value_0(%btree_t_0* %55, [1 x i32]* %value_14_0)
        br label %end_if_5
      end_if_5:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_5_0)
        br label %loop_4
      range_query.end_3:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_4_0)
        br label %loop_3
      range_query.end_2:
        %condition_6_0 = getelementptr %program, %program* %arg_0, i32 0, i32 7
        %condition_6_1 = call ccc i1 @btree_is_empty_0(%btree_t_0* %condition_6_0)
        br i1 %condition_6_1, label %if_6, label %end_if_7
      if_6:
        %condition_7_0 = getelementptr %program, %program* %arg_0, i32 0, i32 8
        %condition_7_1 = call ccc i1 @btree_is_empty_0(%btree_t_0* %condition_7_0)
        br i1 %condition_7_1, label %if_7, label %end_if_6
      if_7:
        br label %loop.end
      end_if_6:
        br label %end_if_7
      end_if_7:
        %begin_iter_6_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_6_0 = alloca %btree_iterator_t_0, i32 1
        %57 = getelementptr %program, %program* %arg_0, i32 0, i32 8
        call ccc void @btree_begin_0(%btree_t_0* %57, %btree_iterator_t_0* %begin_iter_6_0)
        %58 = getelementptr %program, %program* %arg_0, i32 0, i32 8
        call ccc void @btree_end_0(%btree_t_0* %58, %btree_iterator_t_0* %end_iter_6_0)
        %59 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_insert_range_c_new_c(%btree_t_0* %59, %btree_iterator_t_0* %begin_iter_6_0, %btree_iterator_t_0* %end_iter_6_0)
        %60 = getelementptr %program, %program* %arg_0, i32 0, i32 8
        %61 = getelementptr %program, %program* %arg_0, i32 0, i32 6
        call ccc void @btree_swap_0(%btree_t_0* %60, %btree_t_0* %61)
        %begin_iter_7_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_7_0 = alloca %btree_iterator_t_0, i32 1
        %62 = getelementptr %program, %program* %arg_0, i32 0, i32 7
        call ccc void @btree_begin_0(%btree_t_0* %62, %btree_iterator_t_0* %begin_iter_7_0)
        %63 = getelementptr %program, %program* %arg_0, i32 0, i32 7
        call ccc void @btree_end_0(%btree_t_0* %63, %btree_iterator_t_0* %end_iter_7_0)
        %64 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_insert_range_b_new_b(%btree_t_0* %64, %btree_iterator_t_0* %begin_iter_7_0, %btree_iterator_t_0* %end_iter_7_0)
        %65 = getelementptr %program, %program* %arg_0, i32 0, i32 7
        %66 = getelementptr %program, %program* %arg_0, i32 0, i32 5
        call ccc void @btree_swap_0(%btree_t_0* %65, %btree_t_0* %66)
        br label %loop_0
      loop.end:
        %value_15_0 = alloca [1 x i32], i32 1
        %67 = getelementptr [1 x i32], [1 x i32]* %value_15_0, i32 0, i32 0
        store i32 0, i32* %67
        %value_16_0 = alloca [1 x i32], i32 1
        %68 = getelementptr [1 x i32], [1 x i32]* %value_16_0, i32 0, i32 0
        store i32 4294967295, i32* %68
        %begin_iter_8_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_8_0 = alloca %btree_iterator_t_0, i32 1
        %69 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_lower_bound_0(%btree_t_0* %69, [1 x i32]* %value_15_0, %btree_iterator_t_0* %begin_iter_8_0)
        %70 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_upper_bound_0(%btree_t_0* %70, [1 x i32]* %value_16_0, %btree_iterator_t_0* %end_iter_8_0)
        br label %loop_5
      loop_5:
        %condition_8_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_8_0, %btree_iterator_t_0* %end_iter_8_0)
        br i1 %condition_8_0, label %if_8, label %end_if_8
      if_8:
        br label %range_query.end_4
      end_if_8:
        %current_4_0 = call ccc [1 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_8_0)
        %value_17_0 = alloca [1 x i32], i32 1
        %71 = getelementptr [1 x i32], [1 x i32]* %value_17_0, i32 0, i32 0
        %72 = getelementptr [1 x i32], [1 x i32]* %current_4_0, i32 0, i32 0
        %73 = load i32, i32* %72
        store i32 %73, i32* %71
        %value_18_0 = alloca [1 x i32], i32 1
        %74 = getelementptr [1 x i32], [1 x i32]* %value_18_0, i32 0, i32 0
        %75 = getelementptr [1 x i32], [1 x i32]* %current_4_0, i32 0, i32 0
        %76 = load i32, i32* %75
        store i32 %76, i32* %74
        %begin_iter_9_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_9_0 = alloca %btree_iterator_t_0, i32 1
        %77 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_lower_bound_0(%btree_t_0* %77, [1 x i32]* %value_17_0, %btree_iterator_t_0* %begin_iter_9_0)
        %78 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_upper_bound_0(%btree_t_0* %78, [1 x i32]* %value_18_0, %btree_iterator_t_0* %end_iter_9_0)
        br label %loop_6
      loop_6:
        %condition_9_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_9_0, %btree_iterator_t_0* %end_iter_9_0)
        br i1 %condition_9_0, label %if_9, label %end_if_9
      if_9:
        br label %range_query.end_5
      end_if_9:
        %current_5_0 = call ccc [1 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_9_0)
        %value_19_0 = alloca [1 x i32], i32 1
        %79 = getelementptr [1 x i32], [1 x i32]* %value_19_0, i32 0, i32 0
        %80 = getelementptr [1 x i32], [1 x i32]* %current_4_0, i32 0, i32 0
        %81 = load i32, i32* %80
        store i32 %81, i32* %79
        %82 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        %83 = call ccc i1 @btree_insert_value_0(%btree_t_0* %82, [1 x i32]* %value_19_0)
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_9_0)
        br label %loop_6
      range_query.end_5:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_8_0)
        br label %loop_5
      range_query.end_4:
        ret void
      }
      |]
  -- TODO tests for rules with >2 clauses, ...

  it "can generate code for program with no top level facts" $ do
    llvmIR <- cg "no_top_level_facts"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%symbol_table, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc %program* @eclair_program_init() {
      start:
        %memory_0 = call ccc i8* @malloc(i32 1624)
        %program_0 = bitcast i8* %memory_0 to %program*
        %0 = getelementptr %program, %program* %program_0, i32 0, i32 0
        call ccc void @symbol_table_init(%symbol_table* %0)
        %1 = getelementptr %program, %program* %program_0, i32 0, i32 1
        call ccc void @btree_init_empty_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %program_0, i32 0, i32 2
        call ccc void @btree_init_empty_0(%btree_t_0* %2)
        %3 = getelementptr %program, %program* %program_0, i32 0, i32 3
        call ccc void @btree_init_empty_0(%btree_t_0* %3)
        %4 = getelementptr %program, %program* %program_0, i32 0, i32 4
        call ccc void @btree_init_empty_0(%btree_t_0* %4)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc void @eclair_program_destroy(%program* %arg_0) {
      start:
        %0 = getelementptr %program, %program* %arg_0, i32 0, i32 0
        call ccc void @symbol_table_destroy(%symbol_table* %0)
        %1 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_destroy_0(%btree_t_0* %1)
        %2 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_destroy_0(%btree_t_0* %2)
        %3 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_destroy_0(%btree_t_0* %3)
        %4 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_destroy_0(%btree_t_0* %4)
        %memory_0 = bitcast %program* %arg_0 to i8*
        call ccc void @free(i8* %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc void @eclair_program_run(%program* %arg_0) {
      start:
        %value_0 = alloca [2 x i32], i32 1
        %0 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0, i32 0
        store i32 0, i32* %0
        %1 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0, i32 1
        store i32 0, i32* %1
        %value_1_0 = alloca [2 x i32], i32 1
        %2 = getelementptr [2 x i32], [2 x i32]* %value_1_0, i32 0, i32 0
        store i32 4294967295, i32* %2
        %3 = getelementptr [2 x i32], [2 x i32]* %value_1_0, i32 0, i32 1
        store i32 4294967295, i32* %3
        %begin_iter_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_0 = alloca %btree_iterator_t_0, i32 1
        %4 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_lower_bound_0(%btree_t_0* %4, [2 x i32]* %value_0, %btree_iterator_t_0* %begin_iter_0)
        %5 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_upper_bound_0(%btree_t_0* %5, [2 x i32]* %value_1_0, %btree_iterator_t_0* %end_iter_0)
        br label %loop_0
      loop_0:
        %condition_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_0, %btree_iterator_t_0* %end_iter_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 = call ccc [2 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_0)
        %value_2_0 = alloca [2 x i32], i32 1
        %6 = getelementptr [2 x i32], [2 x i32]* %value_2_0, i32 0, i32 0
        %7 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 0
        %8 = load i32, i32* %7
        store i32 %8, i32* %6
        %9 = getelementptr [2 x i32], [2 x i32]* %value_2_0, i32 0, i32 1
        %10 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0, i32 1
        %11 = load i32, i32* %10
        store i32 %11, i32* %9
        %12 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        %13 = call ccc i1 @btree_insert_value_0(%btree_t_0* %12, [2 x i32]* %value_2_0)
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_0)
        br label %loop_0
      range_query.end:
        %begin_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %14 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_begin_0(%btree_t_0* %14, %btree_iterator_t_0* %begin_iter_1_0)
        %15 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_end_0(%btree_t_0* %15, %btree_iterator_t_0* %end_iter_1_0)
        %16 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_insert_range_delta_path_path(%btree_t_0* %16, %btree_iterator_t_0* %begin_iter_1_0, %btree_iterator_t_0* %end_iter_1_0)
        br label %loop_1
      loop_1:
        %17 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_clear_0(%btree_t_0* %17)
        %value_3_0 = alloca [2 x i32], i32 1
        %18 = getelementptr [2 x i32], [2 x i32]* %value_3_0, i32 0, i32 0
        store i32 0, i32* %18
        %19 = getelementptr [2 x i32], [2 x i32]* %value_3_0, i32 0, i32 1
        store i32 0, i32* %19
        %value_4_0 = alloca [2 x i32], i32 1
        %20 = getelementptr [2 x i32], [2 x i32]* %value_4_0, i32 0, i32 0
        store i32 4294967295, i32* %20
        %21 = getelementptr [2 x i32], [2 x i32]* %value_4_0, i32 0, i32 1
        store i32 4294967295, i32* %21
        %begin_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %22 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_lower_bound_0(%btree_t_0* %22, [2 x i32]* %value_3_0, %btree_iterator_t_0* %begin_iter_2_0)
        %23 = getelementptr %program, %program* %arg_0, i32 0, i32 2
        call ccc void @btree_upper_bound_0(%btree_t_0* %23, [2 x i32]* %value_4_0, %btree_iterator_t_0* %end_iter_2_0)
        br label %loop_2
      loop_2:
        %condition_1_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_2_0, %btree_iterator_t_0* %end_iter_2_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 = call ccc [2 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_2_0)
        %value_5_0 = alloca [2 x i32], i32 1
        %24 = getelementptr [2 x i32], [2 x i32]* %value_5_0, i32 0, i32 0
        %25 = getelementptr [2 x i32], [2 x i32]* %current_1_0, i32 0, i32 1
        %26 = load i32, i32* %25
        store i32 %26, i32* %24
        %27 = getelementptr [2 x i32], [2 x i32]* %value_5_0, i32 0, i32 1
        store i32 0, i32* %27
        %value_6_0 = alloca [2 x i32], i32 1
        %28 = getelementptr [2 x i32], [2 x i32]* %value_6_0, i32 0, i32 0
        %29 = getelementptr [2 x i32], [2 x i32]* %current_1_0, i32 0, i32 1
        %30 = load i32, i32* %29
        store i32 %30, i32* %28
        %31 = getelementptr [2 x i32], [2 x i32]* %value_6_0, i32 0, i32 1
        store i32 4294967295, i32* %31
        %begin_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %32 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_lower_bound_0(%btree_t_0* %32, [2 x i32]* %value_5_0, %btree_iterator_t_0* %begin_iter_3_0)
        %33 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_upper_bound_0(%btree_t_0* %33, [2 x i32]* %value_6_0, %btree_iterator_t_0* %end_iter_3_0)
        br label %loop_3
      loop_3:
        %condition_2_0 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %begin_iter_3_0, %btree_iterator_t_0* %end_iter_3_0)
        br i1 %condition_2_0, label %if_2, label %end_if_2
      if_2:
        br label %range_query.end_2
      end_if_2:
        %current_2_0 = call ccc [2 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %begin_iter_3_0)
        %value_7_0 = alloca [2 x i32], i32 1
        %34 = getelementptr [2 x i32], [2 x i32]* %value_7_0, i32 0, i32 0
        %35 = getelementptr [2 x i32], [2 x i32]* %current_1_0, i32 0, i32 0
        %36 = load i32, i32* %35
        store i32 %36, i32* %34
        %37 = getelementptr [2 x i32], [2 x i32]* %value_7_0, i32 0, i32 1
        %38 = getelementptr [2 x i32], [2 x i32]* %current_2_0, i32 0, i32 1
        %39 = load i32, i32* %38
        store i32 %39, i32* %37
        %contains_result_0 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        %contains_result_1 = call ccc i1 @btree_contains_0(%btree_t_0* %contains_result_0, [2 x i32]* %value_7_0)
        %condition_3_0 = select i1 %contains_result_1, i1 0, i1 1
        br i1 %condition_3_0, label %if_3, label %end_if_3
      if_3:
        %value_8_0 = alloca [2 x i32], i32 1
        %40 = getelementptr [2 x i32], [2 x i32]* %value_8_0, i32 0, i32 0
        %41 = getelementptr [2 x i32], [2 x i32]* %current_1_0, i32 0, i32 0
        %42 = load i32, i32* %41
        store i32 %42, i32* %40
        %43 = getelementptr [2 x i32], [2 x i32]* %value_8_0, i32 0, i32 1
        %44 = getelementptr [2 x i32], [2 x i32]* %current_2_0, i32 0, i32 1
        %45 = load i32, i32* %44
        store i32 %45, i32* %43
        %46 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        %47 = call ccc i1 @btree_insert_value_0(%btree_t_0* %46, [2 x i32]* %value_8_0)
        br label %end_if_3
      end_if_3:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_3_0)
        br label %loop_3
      range_query.end_2:
        call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %begin_iter_2_0)
        br label %loop_2
      range_query.end_1:
        %condition_4_0 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        %condition_4_1 = call ccc i1 @btree_is_empty_0(%btree_t_0* %condition_4_0)
        br i1 %condition_4_1, label %if_4, label %end_if_4
      if_4:
        br label %loop.end
      end_if_4:
        %begin_iter_4_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_4_0 = alloca %btree_iterator_t_0, i32 1
        %48 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_begin_0(%btree_t_0* %48, %btree_iterator_t_0* %begin_iter_4_0)
        %49 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        call ccc void @btree_end_0(%btree_t_0* %49, %btree_iterator_t_0* %end_iter_4_0)
        %50 = getelementptr %program, %program* %arg_0, i32 0, i32 4
        call ccc void @btree_insert_range_path_new_path(%btree_t_0* %50, %btree_iterator_t_0* %begin_iter_4_0, %btree_iterator_t_0* %end_iter_4_0)
        %51 = getelementptr %program, %program* %arg_0, i32 0, i32 3
        %52 = getelementptr %program, %program* %arg_0, i32 0, i32 1
        call ccc void @btree_swap_0(%btree_t_0* %51, %btree_t_0* %52)
        br label %loop_1
      loop.end:
        ret void
      }
      |]

  describe "fact IO" $ parallel $ do
    it "only generates IO code for relations visible to the user" $ do
      llvmIR <- cg "no_top_level_facts"
      extractFnSnippet llvmIR "eclair_add_facts" `shouldBe` Just [text|
        define external ccc void @eclair_add_facts(%program* %eclair_program_0, i16 %fact_type_0, i32* %memory_0, i32 %fact_count_0) {
        start:
          switch i16 %fact_type_0, label %switch.default_0 [i16 0, label %edge_0 i16 1, label %path_0]
        edge_0:
          %0 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 2
          %1 = bitcast i32* %memory_0 to [2 x i32]*
          br label %for_begin_0
        for_begin_0:
          %2 = phi i32 [0, %edge_0], [%6, %for_body_0]
          %3 = icmp ult i32 %2, %fact_count_0
          br i1 %3, label %for_body_0, label %for_end_0
        for_body_0:
          %4 = getelementptr [2 x i32], [2 x i32]* %1, i32 %2
          %5 = call ccc i1 @btree_insert_value_0(%btree_t_0* %0, [2 x i32]* %4)
          %6 = add i32 1, %2
          br label %for_begin_0
        for_end_0:
          br label %path_0
        path_0:
          %7 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 4
          %8 = bitcast i32* %memory_0 to [2 x i32]*
          br label %for_begin_1
        for_begin_1:
          %9 = phi i32 [0, %path_0], [%13, %for_body_1]
          %10 = icmp ult i32 %9, %fact_count_0
          br i1 %10, label %for_body_1, label %for_end_1
        for_body_1:
          %11 = getelementptr [2 x i32], [2 x i32]* %8, i32 %9
          %12 = call ccc i1 @btree_insert_value_0(%btree_t_0* %7, [2 x i32]* %11)
          %13 = add i32 1, %9
          br label %for_begin_1
        for_end_1:
          br label %switch.default_0
        switch.default_0:
          ret void
        }
        |]
      extractFnSnippet llvmIR "eclair_get_facts" `shouldBe` Just [text|
        define external ccc i32* @eclair_get_facts(%program* %eclair_program_0, i16 %fact_type_0) {
        start:
          switch i16 %fact_type_0, label %switch.default_0 [i16 0, label %edge_0 i16 1, label %path_0]
        edge_0:
          %0 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 2
          %fact_count_0 = call ccc i64 @btree_size_0(%btree_t_0* %0)
          %fact_count_1 = trunc i64 %fact_count_0 to i32
          %byte_count_0 = mul i32 %fact_count_1, 8
          %memory_0 = call ccc i8* @malloc(i32 %byte_count_0)
          %array_0 = bitcast i8* %memory_0 to [2 x i32]*
          %i_0 = alloca i32, i32 1
          store i32 0, i32* %i_0
          %current_iter_0 = alloca %btree_iterator_t_0, i32 1
          %end_iter_0 = alloca %btree_iterator_t_0, i32 1
          call ccc void @btree_begin_0(%btree_t_0* %0, %btree_iterator_t_0* %current_iter_0)
          call ccc void @btree_end_0(%btree_t_0* %0, %btree_iterator_t_0* %end_iter_0)
          br label %while_begin_0
        while_begin_0:
          %1 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %current_iter_0, %btree_iterator_t_0* %end_iter_0)
          %2 = select i1 %1, i1 0, i1 1
          br i1 %2, label %while_body_0, label %while_end_0
        while_body_0:
          %3 = load i32, i32* %i_0
          %value_0 = getelementptr [2 x i32], [2 x i32]* %array_0, i32 %3
          %current_0 = call ccc [2 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %current_iter_0)
          %4 = getelementptr [2 x i32], [2 x i32]* %current_0, i32 0
          %5 = load [2 x i32], [2 x i32]* %4
          %6 = getelementptr [2 x i32], [2 x i32]* %value_0, i32 0
          store [2 x i32] %5, [2 x i32]* %6
          %7 = add i32 %3, 1
          store i32 %7, i32* %i_0
          call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %current_iter_0)
          br label %while_begin_0
        while_end_0:
          %8 = bitcast i8* %memory_0 to i32*
          ret i32* %8
        path_0:
          %9 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 4
          %fact_count_2 = call ccc i64 @btree_size_0(%btree_t_0* %9)
          %fact_count_3 = trunc i64 %fact_count_2 to i32
          %byte_count_1 = mul i32 %fact_count_3, 8
          %memory_1 = call ccc i8* @malloc(i32 %byte_count_1)
          %array_1 = bitcast i8* %memory_1 to [2 x i32]*
          %i_1 = alloca i32, i32 1
          store i32 0, i32* %i_1
          %current_iter_1 = alloca %btree_iterator_t_0, i32 1
          %end_iter_1 = alloca %btree_iterator_t_0, i32 1
          call ccc void @btree_begin_0(%btree_t_0* %9, %btree_iterator_t_0* %current_iter_1)
          call ccc void @btree_end_0(%btree_t_0* %9, %btree_iterator_t_0* %end_iter_1)
          br label %while_begin_1
        while_begin_1:
          %10 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %current_iter_1, %btree_iterator_t_0* %end_iter_1)
          %11 = select i1 %10, i1 0, i1 1
          br i1 %11, label %while_body_1, label %while_end_1
        while_body_1:
          %12 = load i32, i32* %i_1
          %value_1 = getelementptr [2 x i32], [2 x i32]* %array_1, i32 %12
          %current_1 = call ccc [2 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %current_iter_1)
          %13 = getelementptr [2 x i32], [2 x i32]* %current_1, i32 0
          %14 = load [2 x i32], [2 x i32]* %13
          %15 = getelementptr [2 x i32], [2 x i32]* %value_1, i32 0
          store [2 x i32] %14, [2 x i32]* %15
          %16 = add i32 %12, 1
          store i32 %16, i32* %i_1
          call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %current_iter_1)
          br label %while_begin_1
        while_end_1:
          %17 = bitcast i8* %memory_1 to i32*
          ret i32* %17
        switch.default_0:
          ret i32* zeroinitializer
        }
        |]

    it "generates correct code with facts of different types" $ do
      llvmIR <- cg "different_types"
      extractFnSnippet llvmIR "eclair_add_facts" `shouldBe` Just [text|
        define external ccc void @eclair_add_facts(%program* %eclair_program_0, i16 %fact_type_0, i32* %memory_0, i32 %fact_count_0) {
        start:
          switch i16 %fact_type_0, label %switch.default_0 [i16 0, label %a_0 i16 1, label %b_0]
        a_0:
          %0 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 1
          %1 = bitcast i32* %memory_0 to [1 x i32]*
          br label %for_begin_0
        for_begin_0:
          %2 = phi i32 [0, %a_0], [%6, %for_body_0]
          %3 = icmp ult i32 %2, %fact_count_0
          br i1 %3, label %for_body_0, label %for_end_0
        for_body_0:
          %4 = getelementptr [1 x i32], [1 x i32]* %1, i32 %2
          %5 = call ccc i1 @btree_insert_value_0(%btree_t_0* %0, [1 x i32]* %4)
          %6 = add i32 1, %2
          br label %for_begin_0
        for_end_0:
          br label %b_0
        b_0:
          %7 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 2
          %8 = bitcast i32* %memory_0 to [3 x i32]*
          br label %for_begin_1
        for_begin_1:
          %9 = phi i32 [0, %b_0], [%13, %for_body_1]
          %10 = icmp ult i32 %9, %fact_count_0
          br i1 %10, label %for_body_1, label %for_end_1
        for_body_1:
          %11 = getelementptr [3 x i32], [3 x i32]* %8, i32 %9
          %12 = call ccc i1 @btree_insert_value_1(%btree_t_1* %7, [3 x i32]* %11)
          %13 = add i32 1, %9
          br label %for_begin_1
        for_end_1:
          br label %switch.default_0
        switch.default_0:
          ret void
        }
        |]
      extractFnSnippet llvmIR "eclair_get_facts" `shouldBe` Just [text|
        define external ccc i32* @eclair_get_facts(%program* %eclair_program_0, i16 %fact_type_0) {
        start:
          switch i16 %fact_type_0, label %switch.default_0 [i16 0, label %a_0 i16 1, label %b_0]
        a_0:
          %0 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 1
          %fact_count_0 = call ccc i64 @btree_size_0(%btree_t_0* %0)
          %fact_count_1 = trunc i64 %fact_count_0 to i32
          %byte_count_0 = mul i32 %fact_count_1, 4
          %memory_0 = call ccc i8* @malloc(i32 %byte_count_0)
          %array_0 = bitcast i8* %memory_0 to [1 x i32]*
          %i_0 = alloca i32, i32 1
          store i32 0, i32* %i_0
          %current_iter_0 = alloca %btree_iterator_t_0, i32 1
          %end_iter_0 = alloca %btree_iterator_t_0, i32 1
          call ccc void @btree_begin_0(%btree_t_0* %0, %btree_iterator_t_0* %current_iter_0)
          call ccc void @btree_end_0(%btree_t_0* %0, %btree_iterator_t_0* %end_iter_0)
          br label %while_begin_0
        while_begin_0:
          %1 = call ccc i1 @btree_iterator_is_equal_0(%btree_iterator_t_0* %current_iter_0, %btree_iterator_t_0* %end_iter_0)
          %2 = select i1 %1, i1 0, i1 1
          br i1 %2, label %while_body_0, label %while_end_0
        while_body_0:
          %3 = load i32, i32* %i_0
          %value_0 = getelementptr [1 x i32], [1 x i32]* %array_0, i32 %3
          %current_0 = call ccc [1 x i32]* @btree_iterator_current_0(%btree_iterator_t_0* %current_iter_0)
          %4 = getelementptr [1 x i32], [1 x i32]* %current_0, i32 0
          %5 = load [1 x i32], [1 x i32]* %4
          %6 = getelementptr [1 x i32], [1 x i32]* %value_0, i32 0
          store [1 x i32] %5, [1 x i32]* %6
          %7 = add i32 %3, 1
          store i32 %7, i32* %i_0
          call ccc void @btree_iterator_next_0(%btree_iterator_t_0* %current_iter_0)
          br label %while_begin_0
        while_end_0:
          %8 = bitcast i8* %memory_0 to i32*
          ret i32* %8
        b_0:
          %9 = getelementptr %program, %program* %eclair_program_0, i32 0, i32 2
          %fact_count_2 = call ccc i64 @btree_size_1(%btree_t_1* %9)
          %fact_count_3 = trunc i64 %fact_count_2 to i32
          %byte_count_1 = mul i32 %fact_count_3, 12
          %memory_1 = call ccc i8* @malloc(i32 %byte_count_1)
          %array_1 = bitcast i8* %memory_1 to [3 x i32]*
          %i_1 = alloca i32, i32 1
          store i32 0, i32* %i_1
          %current_iter_1 = alloca %btree_iterator_t_1, i32 1
          %end_iter_1 = alloca %btree_iterator_t_1, i32 1
          call ccc void @btree_begin_1(%btree_t_1* %9, %btree_iterator_t_1* %current_iter_1)
          call ccc void @btree_end_1(%btree_t_1* %9, %btree_iterator_t_1* %end_iter_1)
          br label %while_begin_1
        while_begin_1:
          %10 = call ccc i1 @btree_iterator_is_equal_1(%btree_iterator_t_1* %current_iter_1, %btree_iterator_t_1* %end_iter_1)
          %11 = select i1 %10, i1 0, i1 1
          br i1 %11, label %while_body_1, label %while_end_1
        while_body_1:
          %12 = load i32, i32* %i_1
          %value_1 = getelementptr [3 x i32], [3 x i32]* %array_1, i32 %12
          %current_1 = call ccc [3 x i32]* @btree_iterator_current_1(%btree_iterator_t_1* %current_iter_1)
          %13 = getelementptr [3 x i32], [3 x i32]* %current_1, i32 0
          %14 = load [3 x i32], [3 x i32]* %13
          %15 = getelementptr [3 x i32], [3 x i32]* %value_1, i32 0
          store [3 x i32] %14, [3 x i32]* %15
          %16 = add i32 %12, 1
          store i32 %16, i32* %i_1
          call ccc void @btree_iterator_next_1(%btree_iterator_t_1* %current_iter_1)
          br label %while_begin_1
        while_end_1:
          %17 = bitcast i8* %memory_1 to i32*
          ret i32* %17
        switch.default_0:
          ret i32* zeroinitializer
        }
        |]
