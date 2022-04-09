{-# LANGUAGE TypeFamilies, RankNTypes, QuasiQuotes #-}

module Test.Eclair.LLVM.CodegenSpec
  ( module Test.Eclair.LLVM.CodegenSpec
  ) where

import Protolude hiding ((<.>))
import Control.Arrow ((&&&))
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Eclair
import Eclair.Pretty
import Eclair.Syntax
import System.FilePath
import Test.Hspec
import NeatInterpolation
import LLVM.Pretty


cg :: FilePath -> IO T.Text
cg path = do
  let file = "tests/fixtures/codegen" </> path <.> "dl"
  result <- compileLLVM file
  case result of
    Left err -> panic $ "Failed to parse " <> T.pack file <> "!"
    Right llvm -> pure $ TL.toStrict $ ppllvm llvm

extractDeclTypeSnippet :: Text -> Text
extractDeclTypeSnippet result =
  fromJust $ find (T.isPrefixOf "%program =") $ T.lines result

extractFnSnippet :: Text -> Text -> Maybe Text
extractFnSnippet result fnSignature = do
  let lines = T.lines result
  startLine <- L.findIndex (T.isInfixOf fnSignature) lines
  pure $ T.strip $ T.unlines $ map T.stripEnd $ takeWhile (/= "") $ drop startLine lines

-- TODO add tests for caching mechanism (e.g. single_nonrecursive_rule test)

spec :: Spec
spec = describe "LLVM Code Generation" $ parallel $ do
  it "generates code for a single fact" $ do
    llvmIR <- cg "single_fact"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%btree_t_0, %btree_t_1}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc  %program* @eclair_program_init()    {
        %byte_count_0 = trunc i64 ptrtoint (%program* getelementptr inbounds (%program, %program* inttoptr (i64 0 to %program*), i64 1) to i64) to i32
        %memory_0 =  call ccc  i8*  @malloc(i32  %byte_count_0)
        %program_0 = bitcast i8* %memory_0 to %program*
        %1 = getelementptr  %program, %program* %program_0, i32 0, i32 0
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %program_0, i32 0, i32 1
         call ccc  void  @btree_init_empty_1(%btree_t_1*  %2)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc  void @eclair_program_destroy(%program*  %arg_0)    {
        %1 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_destroy_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_destroy_1(%btree_t_1*  %2)
        %memory_0 = bitcast %program* %arg_0 to i8*
         call ccc  void  @free(i8*  %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc  void @eclair_program_run(%program*  %arg_0)    {
        %value_0 = alloca %value_t_0, i32 1
        %1 = getelementptr  %value_t_0, %value_t_0* %value_0, i32 0, i32 0
        store   i32 1, %column_t_0* %1
        %2 = getelementptr  %value_t_0, %value_t_0* %value_0, i32 0, i32 1
        store   i32 2, %column_t_0* %2
        %3 = getelementptr  %value_t_0, %value_t_0* %value_0, i32 0, i32 2
        store   i32 3, %column_t_0* %3
        %4 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
        %5 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %4, %value_t_0*  %value_0)
        %value_1_0 = alloca %value_t_1, i32 1
        %6 = getelementptr  %value_t_1, %value_t_1* %value_1_0, i32 0, i32 0
        store   i32 2, %column_t_1* %6
        %7 = getelementptr  %value_t_1, %value_t_1* %value_1_0, i32 0, i32 1
        store   i32 3, %column_t_1* %7
        %8 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
        %9 =  call ccc  i1  @btree_insert_value_1(%btree_t_1*  %8, %value_t_1*  %value_1_0)
        %value_2_0 = alloca %value_t_1, i32 1
        %10 = getelementptr  %value_t_1, %value_t_1* %value_2_0, i32 0, i32 0
        store   i32 1, %column_t_1* %10
        %11 = getelementptr  %value_t_1, %value_t_1* %value_2_0, i32 0, i32 1
        store   i32 2, %column_t_1* %11
        %12 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
        %13 =  call ccc  i1  @btree_insert_value_1(%btree_t_1*  %12, %value_t_1*  %value_2_0)
        ret void
      }
      |]

  it "generates code for a single non-recursive rule" $ do
    llvmIR <- cg "single_nonrecursive_rule"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%btree_t_0, %btree_t_0}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc  %program* @eclair_program_init()    {
        %byte_count_0 = trunc i64 ptrtoint (%program* getelementptr inbounds (%program, %program* inttoptr (i64 0 to %program*), i64 1) to i64) to i32
        %memory_0 =  call ccc  i8*  @malloc(i32  %byte_count_0)
        %program_0 = bitcast i8* %memory_0 to %program*
        %1 = getelementptr  %program, %program* %program_0, i32 0, i32 0
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %program_0, i32 0, i32 1
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %2)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc  void @eclair_program_destroy(%program*  %arg_0)    {
        %1 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_destroy_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_destroy_0(%btree_t_0*  %2)
        %memory_0 = bitcast %program* %arg_0 to i8*
         call ccc  void  @free(i8*  %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc  void @eclair_program_run(%program*  %arg_0)    {
      ; <label>:0:
        %value_0 = alloca %value_t_0, i32 1
        %1 = getelementptr  %value_t_0, %value_t_0* %value_0, i32 0, i32 0
        store   i32 1, %column_t_0* %1
        %2 = getelementptr  %value_t_0, %value_t_0* %value_0, i32 0, i32 1
        store   i32 2, %column_t_0* %2
        %3 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
        %4 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %3, %value_t_0*  %value_0)
        %value_1_0 = alloca %value_t_0, i32 1
        %5 = getelementptr  %value_t_0, %value_t_0* %value_1_0, i32 0, i32 0
        store   i32 0, %column_t_0* %5
        %6 = getelementptr  %value_t_0, %value_t_0* %value_1_0, i32 0, i32 1
        store   i32 0, %column_t_0* %6
        %value_2_0 = alloca %value_t_0, i32 1
        %7 = getelementptr  %value_t_0, %value_t_0* %value_2_0, i32 0, i32 0
        store   i32 4294967295, %column_t_0* %7
        %8 = getelementptr  %value_t_0, %value_t_0* %value_2_0, i32 0, i32 1
        store   i32 4294967295, %column_t_0* %8
        %begin_iter_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_0 = alloca %btree_iterator_t_0, i32 1
        %9 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %9, %value_t_0*  %value_1_0, %btree_iterator_t_0*  %begin_iter_0)
        %10 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %10, %value_t_0*  %value_2_0, %btree_iterator_t_0*  %end_iter_0)
        br label %loop_0
      loop_0:
        %condition_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_0, %btree_iterator_t_0*  %end_iter_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_0)
        %value_3_0 = alloca %value_t_0, i32 1
        %11 = getelementptr  %value_t_0, %value_t_0* %value_3_0, i32 0, i32 0
        %12 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %13 = load   %column_t_0, %column_t_0* %12
        store   %column_t_0 %13, %column_t_0* %11
        %14 = getelementptr  %value_t_0, %value_t_0* %value_3_0, i32 0, i32 1
        %15 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 1
        %16 = load   %column_t_0, %column_t_0* %15
        store   %column_t_0 %16, %column_t_0* %14
        %17 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
        %18 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %17, %value_t_0*  %value_3_0)
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_0)
        br label %loop_0
      range_query.end:
        ret void
      }
      |]

  it "generates nested searches correctly" $ do
    llvmIR <- cg "multiple_rule_clauses"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%btree_t_0, %btree_t_1, %btree_t_2}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc  %program* @eclair_program_init()    {
        %byte_count_0 = trunc i64 ptrtoint (%program* getelementptr inbounds (%program, %program* inttoptr (i64 0 to %program*), i64 1) to i64) to i32
        %memory_0 =  call ccc  i8*  @malloc(i32  %byte_count_0)
        %program_0 = bitcast i8* %memory_0 to %program*
        %1 = getelementptr  %program, %program* %program_0, i32 0, i32 0
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %program_0, i32 0, i32 1
         call ccc  void  @btree_init_empty_1(%btree_t_1*  %2)
        %3 = getelementptr  %program, %program* %program_0, i32 0, i32 2
         call ccc  void  @btree_init_empty_2(%btree_t_2*  %3)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc  void @eclair_program_destroy(%program*  %arg_0)    {
        %1 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_destroy_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_destroy_1(%btree_t_1*  %2)
        %3 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_destroy_2(%btree_t_2*  %3)
        %memory_0 = bitcast %program* %arg_0 to i8*
         call ccc  void  @free(i8*  %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc  void @eclair_program_run(%program*  %arg_0)    {
      ; <label>:0:
        %value_0 = alloca %value_t_1, i32 1
        %1 = getelementptr  %value_t_1, %value_t_1* %value_0, i32 0, i32 0
        store   i32 2, %column_t_1* %1
        %2 = getelementptr  %value_t_1, %value_t_1* %value_0, i32 0, i32 1
        store   i32 3, %column_t_1* %2
        %3 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
        %4 =  call ccc  i1  @btree_insert_value_1(%btree_t_1*  %3, %value_t_1*  %value_0)
        %value_1_0 = alloca %value_t_0, i32 1
        %5 = getelementptr  %value_t_0, %value_t_0* %value_1_0, i32 0, i32 0
        store   i32 1, %column_t_0* %5
        %6 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
        %7 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %6, %value_t_0*  %value_1_0)
        %value_2_0 = alloca %value_t_0, i32 1
        %8 = getelementptr  %value_t_0, %value_t_0* %value_2_0, i32 0, i32 0
        store   i32 0, %column_t_0* %8
        %value_3_0 = alloca %value_t_0, i32 1
        %9 = getelementptr  %value_t_0, %value_t_0* %value_3_0, i32 0, i32 0
        store   i32 4294967295, %column_t_0* %9
        %begin_iter_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_0 = alloca %btree_iterator_t_0, i32 1
        %10 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %10, %value_t_0*  %value_2_0, %btree_iterator_t_0*  %begin_iter_0)
        %11 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %11, %value_t_0*  %value_3_0, %btree_iterator_t_0*  %end_iter_0)
        br label %loop_0
      loop_0:
        %condition_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_0, %btree_iterator_t_0*  %end_iter_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_0)
        %value_4_0 = alloca %value_t_1, i32 1
        %12 = getelementptr  %value_t_1, %value_t_1* %value_4_0, i32 0, i32 0
        store   i32 0, %column_t_1* %12
        %13 = getelementptr  %value_t_1, %value_t_1* %value_4_0, i32 0, i32 1
        %14 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %15 = load   %column_t_0, %column_t_0* %14
        store   %column_t_0 %15, %column_t_1* %13
        %value_5_0 = alloca %value_t_1, i32 1
        %16 = getelementptr  %value_t_1, %value_t_1* %value_5_0, i32 0, i32 0
        store   i32 4294967295, %column_t_1* %16
        %17 = getelementptr  %value_t_1, %value_t_1* %value_5_0, i32 0, i32 1
        %18 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %19 = load   %column_t_0, %column_t_0* %18
        store   %column_t_0 %19, %column_t_1* %17
        %begin_iter_1_0 = alloca %btree_iterator_t_1, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_1, i32 1
        %20 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_lower_bound_1(%btree_t_1*  %20, %value_t_1*  %value_4_0, %btree_iterator_t_1*  %begin_iter_1_0)
        %21 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_upper_bound_1(%btree_t_1*  %21, %value_t_1*  %value_5_0, %btree_iterator_t_1*  %end_iter_1_0)
        br label %loop_1
      loop_1:
        %condition_1_0 =  call ccc  i1  @btree_iterator_is_equal_1(%btree_iterator_t_1*  %begin_iter_1_0, %btree_iterator_t_1*  %end_iter_1_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 =  call ccc  %value_t_1*  @btree_iterator_current_1(%btree_iterator_t_1*  %begin_iter_1_0)
        %condition_2_0 = getelementptr  %value_t_1, %value_t_1* %current_1_0, i32 0, i32 1
        %condition_2_1 = load   %column_t_1, %column_t_1* %condition_2_0
        %condition_2_2 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %condition_2_3 = load   %column_t_0, %column_t_0* %condition_2_2
        %condition_2_4 = icmp eq %column_t_1 %condition_2_1, %condition_2_3
        br i1 %condition_2_4, label %if_2, label %end_if_2
      if_2:
        %value_6_0 = alloca %value_t_2, i32 1
        %22 = getelementptr  %value_t_2, %value_t_2* %value_6_0, i32 0, i32 0
        %23 = getelementptr  %value_t_1, %value_t_1* %current_1_0, i32 0, i32 0
        %24 = load   %column_t_1, %column_t_1* %23
        store   %column_t_1 %24, %column_t_2* %22
        %25 = getelementptr  %value_t_2, %value_t_2* %value_6_0, i32 0, i32 1
        %26 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %27 = load   %column_t_0, %column_t_0* %26
        store   %column_t_0 %27, %column_t_2* %25
        %28 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
        %29 =  call ccc  i1  @btree_insert_value_2(%btree_t_2*  %28, %value_t_2*  %value_6_0)
        br label %end_if_2
      end_if_2:
         call ccc  void  @btree_iterator_next_1(%btree_iterator_t_1*  %begin_iter_1_0)
        br label %loop_1
      range_query.end_1:
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_0)
        br label %loop_0
      range_query.end:
        ret void
      }
      |]

  it "generates code for a rule with 2 clauses of same name" $ do
    llvmIR <- cg "multiple_clauses_same_name"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%btree_t_0, %btree_t_1}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc  %program* @eclair_program_init()    {
        %byte_count_0 = trunc i64 ptrtoint (%program* getelementptr inbounds (%program, %program* inttoptr (i64 0 to %program*), i64 1) to i64) to i32
        %memory_0 =  call ccc  i8*  @malloc(i32  %byte_count_0)
        %program_0 = bitcast i8* %memory_0 to %program*
        %1 = getelementptr  %program, %program* %program_0, i32 0, i32 0
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %program_0, i32 0, i32 1
         call ccc  void  @btree_init_empty_1(%btree_t_1*  %2)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc  void @eclair_program_destroy(%program*  %arg_0)    {
        %1 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_destroy_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_destroy_1(%btree_t_1*  %2)
        %memory_0 = bitcast %program* %arg_0 to i8*
         call ccc  void  @free(i8*  %memory_0)
        ret void
      }
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      define external ccc  void @eclair_program_run(%program*  %arg_0)    {
      ; <label>:0:
        %value_0 = alloca %value_t_1, i32 1
        %1 = getelementptr  %value_t_1, %value_t_1* %value_0, i32 0, i32 0
        store   i32 1, %column_t_1* %1
        %2 = getelementptr  %value_t_1, %value_t_1* %value_0, i32 0, i32 1
        store   i32 2, %column_t_1* %2
        %3 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
        %4 =  call ccc  i1  @btree_insert_value_1(%btree_t_1*  %3, %value_t_1*  %value_0)
        %value_1_0 = alloca %value_t_1, i32 1
        %5 = getelementptr  %value_t_1, %value_t_1* %value_1_0, i32 0, i32 0
        store   i32 0, %column_t_1* %5
        %6 = getelementptr  %value_t_1, %value_t_1* %value_1_0, i32 0, i32 1
        store   i32 0, %column_t_1* %6
        %value_2_0 = alloca %value_t_1, i32 1
        %7 = getelementptr  %value_t_1, %value_t_1* %value_2_0, i32 0, i32 0
        store   i32 4294967295, %column_t_1* %7
        %8 = getelementptr  %value_t_1, %value_t_1* %value_2_0, i32 0, i32 1
        store   i32 4294967295, %column_t_1* %8
        %begin_iter_0 = alloca %btree_iterator_t_1, i32 1
        %end_iter_0 = alloca %btree_iterator_t_1, i32 1
        %9 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_lower_bound_1(%btree_t_1*  %9, %value_t_1*  %value_1_0, %btree_iterator_t_1*  %begin_iter_0)
        %10 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_upper_bound_1(%btree_t_1*  %10, %value_t_1*  %value_2_0, %btree_iterator_t_1*  %end_iter_0)
        br label %loop_0
      loop_0:
        %condition_0 =  call ccc  i1  @btree_iterator_is_equal_1(%btree_iterator_t_1*  %begin_iter_0, %btree_iterator_t_1*  %end_iter_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 =  call ccc  %value_t_1*  @btree_iterator_current_1(%btree_iterator_t_1*  %begin_iter_0)
        %value_3_0 = alloca %value_t_1, i32 1
        %11 = getelementptr  %value_t_1, %value_t_1* %value_3_0, i32 0, i32 0
        %12 = getelementptr  %value_t_1, %value_t_1* %current_0, i32 0, i32 1
        %13 = load   %column_t_1, %column_t_1* %12
        store   %column_t_1 %13, %column_t_1* %11
        %14 = getelementptr  %value_t_1, %value_t_1* %value_3_0, i32 0, i32 1
        store   i32 0, %column_t_1* %14
        %value_4_0 = alloca %value_t_1, i32 1
        %15 = getelementptr  %value_t_1, %value_t_1* %value_4_0, i32 0, i32 0
        %16 = getelementptr  %value_t_1, %value_t_1* %current_0, i32 0, i32 1
        %17 = load   %column_t_1, %column_t_1* %16
        store   %column_t_1 %17, %column_t_1* %15
        %18 = getelementptr  %value_t_1, %value_t_1* %value_4_0, i32 0, i32 1
        store   i32 4294967295, %column_t_1* %18
        %begin_iter_1_0 = alloca %btree_iterator_t_1, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_1, i32 1
        %19 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_lower_bound_1(%btree_t_1*  %19, %value_t_1*  %value_3_0, %btree_iterator_t_1*  %begin_iter_1_0)
        %20 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_upper_bound_1(%btree_t_1*  %20, %value_t_1*  %value_4_0, %btree_iterator_t_1*  %end_iter_1_0)
        br label %loop_1
      loop_1:
        %condition_1_0 =  call ccc  i1  @btree_iterator_is_equal_1(%btree_iterator_t_1*  %begin_iter_1_0, %btree_iterator_t_1*  %end_iter_1_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 =  call ccc  %value_t_1*  @btree_iterator_current_1(%btree_iterator_t_1*  %begin_iter_1_0)
        %condition_2_0 = getelementptr  %value_t_1, %value_t_1* %current_1_0, i32 0, i32 0
        %condition_2_1 = load   %column_t_1, %column_t_1* %condition_2_0
        %condition_2_2 = getelementptr  %value_t_1, %value_t_1* %current_0, i32 0, i32 1
        %condition_2_3 = load   %column_t_1, %column_t_1* %condition_2_2
        %condition_2_4 = icmp eq %column_t_1 %condition_2_1, %condition_2_3
        br i1 %condition_2_4, label %if_2, label %end_if_2
      if_2:
        %value_5_0 = alloca %value_t_0, i32 1
        %21 = getelementptr  %value_t_0, %value_t_0* %value_5_0, i32 0, i32 0
        %22 = getelementptr  %value_t_1, %value_t_1* %current_0, i32 0, i32 0
        %23 = load   %column_t_1, %column_t_1* %22
        store   %column_t_1 %23, %column_t_0* %21
        %24 = getelementptr  %value_t_0, %value_t_0* %value_5_0, i32 0, i32 1
        %25 = getelementptr  %value_t_1, %value_t_1* %current_0, i32 0, i32 1
        %26 = load   %column_t_1, %column_t_1* %25
        store   %column_t_1 %26, %column_t_0* %24
        %27 = getelementptr  %value_t_0, %value_t_0* %value_5_0, i32 0, i32 2
        %28 = getelementptr  %value_t_1, %value_t_1* %current_1_0, i32 0, i32 1
        %29 = load   %column_t_1, %column_t_1* %28
        store   %column_t_1 %29, %column_t_0* %27
        %30 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
        %31 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %30, %value_t_0*  %value_5_0)
        br label %end_if_2
      end_if_2:
         call ccc  void  @btree_iterator_next_1(%btree_iterator_t_1*  %begin_iter_1_0)
        br label %loop_1
      range_query.end_1:
         call ccc  void  @btree_iterator_next_1(%btree_iterator_t_1*  %begin_iter_0)
        br label %loop_0
      range_query.end:
        ret void
      }
      |]

  {-
  it "generates code for a rule where columns need to equal each other" $ do
    pending -- TODO: cg "rule_equal_columns"

  it "generates code for a single recursive rule" $ do
    llvmIR <- cg "single_recursive_rule"
    -- NOTE: program for now also contains delta_ and new_ relations,
    -- probably it's more efficient to move these to the stack (but left out of scope for now)
    extractDeclTypeSnippet llvmIR `shouldBe` [text|
      |]
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      |]

  -- TODO variant where one is recursive
  it "generates code for mutually recursive rules" $ do
    llvmIR <- cg "mutually_recursive_rules"
    -- NOTE: program for now also contains delta_ and new_ relations,
    -- probably it's more efficient to move these to the stack (but left out of scope for now)
    extractDeclTypeSnippet llvmIR `shouldBe` [text|
      |]
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      |]
-}
  -- TODO tests for rules with >2 clauses, ...
