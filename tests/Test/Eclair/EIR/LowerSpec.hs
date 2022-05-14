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
import System.FilePath
import Test.Hspec
import NeatInterpolation
import LLVM.Pretty

-- Tip: compare LLVM IR with EIR from tests that generate pretty-printed EIR


cg :: FilePath -> IO T.Text
cg path = do
  let file = "tests/fixtures" </> path <.> "dl"
  llvm <- compileLLVM file
  pure $ toStrict $ ppllvm llvm

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
    llvmIR <- cg "empty"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc  %program* @eclair_program_init()    {
        %byte_count_0 = trunc i64 ptrtoint (%program* getelementptr inbounds (%program, %program* inttoptr (i64 0 to %program*), i64 1) to i64) to i32
        %memory_0 =  call ccc  i8*  @malloc(i32  %byte_count_0)
        %program_0 = bitcast i8* %memory_0 to %program*
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc  void @eclair_program_destroy(%program*  %arg_0)    {
        %memory_0 = bitcast %program* %arg_0 to i8*
         call ccc  void  @free(i8*  %memory_0)
        ret void
      }
      |]
    -- It generates an empty function (forward decl?), but apparently LLVM is fine with it, huh.
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      declare external ccc  void @eclair_program_run(%program*)
      |]

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
         call ccc  void  @btree_iterator_next_1(%btree_iterator_t_1*  %begin_iter_1_0)
        br label %loop_1
      range_query.end_1:
         call ccc  void  @btree_iterator_next_1(%btree_iterator_t_1*  %begin_iter_0)
        br label %loop_0
      range_query.end:
        ret void
      }
      |]

  it "generates code for a rule where columns need to equal each other" $ do
    pending -- TODO: cg "rule_equal_columns"

  it "generates code for a single recursive rule" $ do
    llvmIR <- cg "single_recursive_rule"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc  %program* @eclair_program_init()    {
        %byte_count_0 = trunc i64 ptrtoint (%program* getelementptr inbounds (%program, %program* inttoptr (i64 0 to %program*), i64 1) to i64) to i32
        %memory_0 =  call ccc  i8*  @malloc(i32  %byte_count_0)
        %program_0 = bitcast i8* %memory_0 to %program*
        %1 = getelementptr  %program, %program* %program_0, i32 0, i32 0
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %program_0, i32 0, i32 1
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %2)
        %3 = getelementptr  %program, %program* %program_0, i32 0, i32 2
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %3)
        %4 = getelementptr  %program, %program* %program_0, i32 0, i32 3
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %4)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc  void @eclair_program_destroy(%program*  %arg_0)    {
        %1 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_destroy_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_destroy_0(%btree_t_0*  %2)
        %3 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_destroy_0(%btree_t_0*  %3)
        %4 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_destroy_0(%btree_t_0*  %4)
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
        %3 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
        %4 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %3, %value_t_0*  %value_0)
        %begin_iter_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_0 = alloca %btree_iterator_t_0, i32 1
        %5 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_begin_0(%btree_t_0*  %5, %btree_iterator_t_0*  %begin_iter_0)
        %6 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_end_0(%btree_t_0*  %6, %btree_iterator_t_0*  %end_iter_0)
        %7 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_insert_range_0(%btree_t_0*  %7, %btree_iterator_t_0*  %begin_iter_0, %btree_iterator_t_0*  %end_iter_0)
        br label %loop_0
      loop_0:
        %8 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_clear_0(%btree_t_0*  %8)
        %value_1_0 = alloca %value_t_0, i32 1
        %9 = getelementptr  %value_t_0, %value_t_0* %value_1_0, i32 0, i32 0
        store   i32 0, %column_t_0* %9
        %10 = getelementptr  %value_t_0, %value_t_0* %value_1_0, i32 0, i32 1
        store   i32 0, %column_t_0* %10
        %value_2_0 = alloca %value_t_0, i32 1
        %11 = getelementptr  %value_t_0, %value_t_0* %value_2_0, i32 0, i32 0
        store   i32 4294967295, %column_t_0* %11
        %12 = getelementptr  %value_t_0, %value_t_0* %value_2_0, i32 0, i32 1
        store   i32 4294967295, %column_t_0* %12
        %begin_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %13 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %13, %value_t_0*  %value_1_0, %btree_iterator_t_0*  %begin_iter_1_0)
        %14 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %14, %value_t_0*  %value_2_0, %btree_iterator_t_0*  %end_iter_1_0)
        br label %loop_1
      loop_1:
        %condition_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_1_0, %btree_iterator_t_0*  %end_iter_1_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_1_0)
        %value_3_0 = alloca %value_t_0, i32 1
        %15 = getelementptr  %value_t_0, %value_t_0* %value_3_0, i32 0, i32 0
        %16 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 1
        %17 = load   %column_t_0, %column_t_0* %16
        store   %column_t_0 %17, %column_t_0* %15
        %18 = getelementptr  %value_t_0, %value_t_0* %value_3_0, i32 0, i32 1
        store   i32 0, %column_t_0* %18
        %value_4_0 = alloca %value_t_0, i32 1
        %19 = getelementptr  %value_t_0, %value_t_0* %value_4_0, i32 0, i32 0
        %20 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 1
        %21 = load   %column_t_0, %column_t_0* %20
        store   %column_t_0 %21, %column_t_0* %19
        %22 = getelementptr  %value_t_0, %value_t_0* %value_4_0, i32 0, i32 1
        store   i32 4294967295, %column_t_0* %22
        %begin_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %23 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %23, %value_t_0*  %value_3_0, %btree_iterator_t_0*  %begin_iter_2_0)
        %24 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %24, %value_t_0*  %value_4_0, %btree_iterator_t_0*  %end_iter_2_0)
        br label %loop_2
      loop_2:
        %condition_1_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_2_0, %btree_iterator_t_0*  %end_iter_2_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_2_0)
        %value_5_0 = alloca %value_t_0, i32 1
        %25 = getelementptr  %value_t_0, %value_t_0* %value_5_0, i32 0, i32 0
        %26 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %27 = load   %column_t_0, %column_t_0* %26
        store   %column_t_0 %27, %column_t_0* %25
        %28 = getelementptr  %value_t_0, %value_t_0* %value_5_0, i32 0, i32 1
        %29 = getelementptr  %value_t_0, %value_t_0* %current_1_0, i32 0, i32 1
        %30 = load   %column_t_0, %column_t_0* %29
        store   %column_t_0 %30, %column_t_0* %28
        %contains_result_0 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
        %contains_result_1 =  call ccc  i1  @btree_contains_0(%btree_t_0*  %contains_result_0, %value_t_0*  %value_5_0)
        %condition_2_0 = select i1 %contains_result_1, i1 0, i1 1
        br i1 %condition_2_0, label %if_2, label %end_if_2
      if_2:
        %value_6_0 = alloca %value_t_0, i32 1
        %31 = getelementptr  %value_t_0, %value_t_0* %value_6_0, i32 0, i32 0
        %32 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %33 = load   %column_t_0, %column_t_0* %32
        store   %column_t_0 %33, %column_t_0* %31
        %34 = getelementptr  %value_t_0, %value_t_0* %value_6_0, i32 0, i32 1
        %35 = getelementptr  %value_t_0, %value_t_0* %current_1_0, i32 0, i32 1
        %36 = load   %column_t_0, %column_t_0* %35
        store   %column_t_0 %36, %column_t_0* %34
        %37 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
        %38 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %37, %value_t_0*  %value_6_0)
        br label %end_if_2
      end_if_2:
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_2_0)
        br label %loop_2
      range_query.end_1:
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_1_0)
        br label %loop_1
      range_query.end:
        %condition_3_0 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
        %condition_3_1 =  call ccc  i1  @btree_is_empty_0(%btree_t_0*  %condition_3_0)
        br i1 %condition_3_1, label %if_3, label %end_if_3
      if_3:
        br label %loop.end
      end_if_3:
        %begin_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %39 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_begin_0(%btree_t_0*  %39, %btree_iterator_t_0*  %begin_iter_3_0)
        %40 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_end_0(%btree_t_0*  %40, %btree_iterator_t_0*  %end_iter_3_0)
        %41 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_insert_range_0(%btree_t_0*  %41, %btree_iterator_t_0*  %begin_iter_3_0, %btree_iterator_t_0*  %end_iter_3_0)
        %42 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
        %43 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_swap_0(%btree_t_0*  %42, %btree_t_0*  %43)
        br label %loop_0
      loop.end:
        ret void
      }
      |]

  -- TODO variant where one is recursive
  it "generates code for mutually recursive rules" $ do
    llvmIR <- cg "mutually_recursive_rules"
    extractDeclTypeSnippet llvmIR `shouldBe`
      "%program = type {%btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc  %program* @eclair_program_init()    {
        %byte_count_0 = trunc i64 ptrtoint (%program* getelementptr inbounds (%program, %program* inttoptr (i64 0 to %program*), i64 1) to i64) to i32
        %memory_0 =  call ccc  i8*  @malloc(i32  %byte_count_0)
        %program_0 = bitcast i8* %memory_0 to %program*
        %1 = getelementptr  %program, %program* %program_0, i32 0, i32 0
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %program_0, i32 0, i32 1
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %2)
        %3 = getelementptr  %program, %program* %program_0, i32 0, i32 2
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %3)
        %4 = getelementptr  %program, %program* %program_0, i32 0, i32 3
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %4)
        %5 = getelementptr  %program, %program* %program_0, i32 0, i32 4
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %5)
        %6 = getelementptr  %program, %program* %program_0, i32 0, i32 5
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %6)
        %7 = getelementptr  %program, %program* %program_0, i32 0, i32 6
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %7)
        %8 = getelementptr  %program, %program* %program_0, i32 0, i32 7
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %8)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc  void @eclair_program_destroy(%program*  %arg_0)    {
        %1 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_destroy_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_destroy_0(%btree_t_0*  %2)
        %3 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_destroy_0(%btree_t_0*  %3)
        %4 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_destroy_0(%btree_t_0*  %4)
        %5 = getelementptr  %program, %program* %arg_0, i32 0, i32 4
         call ccc  void  @btree_destroy_0(%btree_t_0*  %5)
        %6 = getelementptr  %program, %program* %arg_0, i32 0, i32 5
         call ccc  void  @btree_destroy_0(%btree_t_0*  %6)
        %7 = getelementptr  %program, %program* %arg_0, i32 0, i32 6
         call ccc  void  @btree_destroy_0(%btree_t_0*  %7)
        %8 = getelementptr  %program, %program* %arg_0, i32 0, i32 7
         call ccc  void  @btree_destroy_0(%btree_t_0*  %8)
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
        store   i32 3, %column_t_0* %1
        %2 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
        %3 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %2, %value_t_0*  %value_0)
        %value_1_0 = alloca %value_t_0, i32 1
        %4 = getelementptr  %value_t_0, %value_t_0* %value_1_0, i32 0, i32 0
        store   i32 2, %column_t_0* %4
        %5 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
        %6 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %5, %value_t_0*  %value_1_0)
        %value_2_0 = alloca %value_t_0, i32 1
        %7 = getelementptr  %value_t_0, %value_t_0* %value_2_0, i32 0, i32 0
        store   i32 1, %column_t_0* %7
        %8 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
        %9 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %8, %value_t_0*  %value_2_0)
        %begin_iter_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_0 = alloca %btree_iterator_t_0, i32 1
        %10 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_begin_0(%btree_t_0*  %10, %btree_iterator_t_0*  %begin_iter_0)
        %11 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_end_0(%btree_t_0*  %11, %btree_iterator_t_0*  %end_iter_0)
        %12 = getelementptr  %program, %program* %arg_0, i32 0, i32 5
         call ccc  void  @btree_insert_range_0(%btree_t_0*  %12, %btree_iterator_t_0*  %begin_iter_0, %btree_iterator_t_0*  %end_iter_0)
        %begin_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %13 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_begin_0(%btree_t_0*  %13, %btree_iterator_t_0*  %begin_iter_1_0)
        %14 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_end_0(%btree_t_0*  %14, %btree_iterator_t_0*  %end_iter_1_0)
        %15 = getelementptr  %program, %program* %arg_0, i32 0, i32 4
         call ccc  void  @btree_insert_range_0(%btree_t_0*  %15, %btree_iterator_t_0*  %begin_iter_1_0, %btree_iterator_t_0*  %end_iter_1_0)
        br label %loop_0
      loop_0:
        %16 = getelementptr  %program, %program* %arg_0, i32 0, i32 7
         call ccc  void  @btree_clear_0(%btree_t_0*  %16)
        %17 = getelementptr  %program, %program* %arg_0, i32 0, i32 6
         call ccc  void  @btree_clear_0(%btree_t_0*  %17)
        %value_3_0 = alloca %value_t_0, i32 1
        %18 = getelementptr  %value_t_0, %value_t_0* %value_3_0, i32 0, i32 0
        store   i32 0, %column_t_0* %18
        %value_4_0 = alloca %value_t_0, i32 1
        %19 = getelementptr  %value_t_0, %value_t_0* %value_4_0, i32 0, i32 0
        store   i32 4294967295, %column_t_0* %19
        %begin_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %20 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %20, %value_t_0*  %value_3_0, %btree_iterator_t_0*  %begin_iter_2_0)
        %21 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %21, %value_t_0*  %value_4_0, %btree_iterator_t_0*  %end_iter_2_0)
        br label %loop_1
      loop_1:
        %condition_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_2_0, %btree_iterator_t_0*  %end_iter_2_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_2_0)
        %value_5_0 = alloca %value_t_0, i32 1
        %22 = getelementptr  %value_t_0, %value_t_0* %value_5_0, i32 0, i32 0
        %23 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %24 = load   %column_t_0, %column_t_0* %23
        store   %column_t_0 %24, %column_t_0* %22
        %value_6_0 = alloca %value_t_0, i32 1
        %25 = getelementptr  %value_t_0, %value_t_0* %value_6_0, i32 0, i32 0
        %26 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %27 = load   %column_t_0, %column_t_0* %26
        store   %column_t_0 %27, %column_t_0* %25
        %begin_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %28 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %28, %value_t_0*  %value_5_0, %btree_iterator_t_0*  %begin_iter_3_0)
        %29 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %29, %value_t_0*  %value_6_0, %btree_iterator_t_0*  %end_iter_3_0)
        br label %loop_2
      loop_2:
        %condition_1_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_3_0, %btree_iterator_t_0*  %end_iter_3_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_3_0)
        %value_7_0 = alloca %value_t_0, i32 1
        %30 = getelementptr  %value_t_0, %value_t_0* %value_7_0, i32 0, i32 0
        %31 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %32 = load   %column_t_0, %column_t_0* %31
        store   %column_t_0 %32, %column_t_0* %30
        %contains_result_0 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
        %contains_result_1 =  call ccc  i1  @btree_contains_0(%btree_t_0*  %contains_result_0, %value_t_0*  %value_7_0)
        %condition_2_0 = select i1 %contains_result_1, i1 0, i1 1
        br i1 %condition_2_0, label %if_2, label %end_if_2
      if_2:
        %value_8_0 = alloca %value_t_0, i32 1
        %33 = getelementptr  %value_t_0, %value_t_0* %value_8_0, i32 0, i32 0
        %34 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %35 = load   %column_t_0, %column_t_0* %34
        store   %column_t_0 %35, %column_t_0* %33
        %36 = getelementptr  %program, %program* %arg_0, i32 0, i32 7
        %37 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %36, %value_t_0*  %value_8_0)
        br label %end_if_2
      end_if_2:
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_3_0)
        br label %loop_2
      range_query.end_1:
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_2_0)
        br label %loop_1
      range_query.end:
        %value_9_0 = alloca %value_t_0, i32 1
        %38 = getelementptr  %value_t_0, %value_t_0* %value_9_0, i32 0, i32 0
        store   i32 0, %column_t_0* %38
        %value_10_0 = alloca %value_t_0, i32 1
        %39 = getelementptr  %value_t_0, %value_t_0* %value_10_0, i32 0, i32 0
        store   i32 4294967295, %column_t_0* %39
        %begin_iter_4_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_4_0 = alloca %btree_iterator_t_0, i32 1
        %40 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %40, %value_t_0*  %value_9_0, %btree_iterator_t_0*  %begin_iter_4_0)
        %41 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %41, %value_t_0*  %value_10_0, %btree_iterator_t_0*  %end_iter_4_0)
        br label %loop_3
      loop_3:
        %condition_3_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_4_0, %btree_iterator_t_0*  %end_iter_4_0)
        br i1 %condition_3_0, label %if_3, label %end_if_3
      if_3:
        br label %range_query.end_2
      end_if_3:
        %current_2_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_4_0)
        %value_11_0 = alloca %value_t_0, i32 1
        %42 = getelementptr  %value_t_0, %value_t_0* %value_11_0, i32 0, i32 0
        %43 = getelementptr  %value_t_0, %value_t_0* %current_2_0, i32 0, i32 0
        %44 = load   %column_t_0, %column_t_0* %43
        store   %column_t_0 %44, %column_t_0* %42
        %value_12_0 = alloca %value_t_0, i32 1
        %45 = getelementptr  %value_t_0, %value_t_0* %value_12_0, i32 0, i32 0
        %46 = getelementptr  %value_t_0, %value_t_0* %current_2_0, i32 0, i32 0
        %47 = load   %column_t_0, %column_t_0* %46
        store   %column_t_0 %47, %column_t_0* %45
        %begin_iter_5_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_5_0 = alloca %btree_iterator_t_0, i32 1
        %48 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %48, %value_t_0*  %value_11_0, %btree_iterator_t_0*  %begin_iter_5_0)
        %49 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %49, %value_t_0*  %value_12_0, %btree_iterator_t_0*  %end_iter_5_0)
        br label %loop_4
      loop_4:
        %condition_4_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_5_0, %btree_iterator_t_0*  %end_iter_5_0)
        br i1 %condition_4_0, label %if_4, label %end_if_4
      if_4:
        br label %range_query.end_3
      end_if_4:
        %current_3_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_5_0)
        %value_13_0 = alloca %value_t_0, i32 1
        %50 = getelementptr  %value_t_0, %value_t_0* %value_13_0, i32 0, i32 0
        %51 = getelementptr  %value_t_0, %value_t_0* %current_2_0, i32 0, i32 0
        %52 = load   %column_t_0, %column_t_0* %51
        store   %column_t_0 %52, %column_t_0* %50
        %contains_result_1_0 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
        %contains_result_1_1 =  call ccc  i1  @btree_contains_0(%btree_t_0*  %contains_result_1_0, %value_t_0*  %value_13_0)
        %condition_5_0 = select i1 %contains_result_1_1, i1 0, i1 1
        br i1 %condition_5_0, label %if_5, label %end_if_5
      if_5:
        %value_14_0 = alloca %value_t_0, i32 1
        %53 = getelementptr  %value_t_0, %value_t_0* %value_14_0, i32 0, i32 0
        %54 = getelementptr  %value_t_0, %value_t_0* %current_2_0, i32 0, i32 0
        %55 = load   %column_t_0, %column_t_0* %54
        store   %column_t_0 %55, %column_t_0* %53
        %56 = getelementptr  %program, %program* %arg_0, i32 0, i32 6
        %57 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %56, %value_t_0*  %value_14_0)
        br label %end_if_5
      end_if_5:
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_5_0)
        br label %loop_4
      range_query.end_3:
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_4_0)
        br label %loop_3
      range_query.end_2:
        %condition_6_0 = getelementptr  %program, %program* %arg_0, i32 0, i32 6
        %condition_6_1 =  call ccc  i1  @btree_is_empty_0(%btree_t_0*  %condition_6_0)
        br i1 %condition_6_1, label %if_6, label %end_if_7
      if_6:
        %condition_7_0 = getelementptr  %program, %program* %arg_0, i32 0, i32 7
        %condition_7_1 =  call ccc  i1  @btree_is_empty_0(%btree_t_0*  %condition_7_0)
        br i1 %condition_7_1, label %if_7, label %end_if_6
      if_7:
        br label %loop.end
      end_if_6:
        br label %end_if_7
      end_if_7:
        %begin_iter_6_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_6_0 = alloca %btree_iterator_t_0, i32 1
        %58 = getelementptr  %program, %program* %arg_0, i32 0, i32 7
         call ccc  void  @btree_begin_0(%btree_t_0*  %58, %btree_iterator_t_0*  %begin_iter_6_0)
        %59 = getelementptr  %program, %program* %arg_0, i32 0, i32 7
         call ccc  void  @btree_end_0(%btree_t_0*  %59, %btree_iterator_t_0*  %end_iter_6_0)
        %60 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_insert_range_0(%btree_t_0*  %60, %btree_iterator_t_0*  %begin_iter_6_0, %btree_iterator_t_0*  %end_iter_6_0)
        %61 = getelementptr  %program, %program* %arg_0, i32 0, i32 7
        %62 = getelementptr  %program, %program* %arg_0, i32 0, i32 5
         call ccc  void  @btree_swap_0(%btree_t_0*  %61, %btree_t_0*  %62)
        %begin_iter_7_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_7_0 = alloca %btree_iterator_t_0, i32 1
        %63 = getelementptr  %program, %program* %arg_0, i32 0, i32 6
         call ccc  void  @btree_begin_0(%btree_t_0*  %63, %btree_iterator_t_0*  %begin_iter_7_0)
        %64 = getelementptr  %program, %program* %arg_0, i32 0, i32 6
         call ccc  void  @btree_end_0(%btree_t_0*  %64, %btree_iterator_t_0*  %end_iter_7_0)
        %65 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_insert_range_0(%btree_t_0*  %65, %btree_iterator_t_0*  %begin_iter_7_0, %btree_iterator_t_0*  %end_iter_7_0)
        %66 = getelementptr  %program, %program* %arg_0, i32 0, i32 6
        %67 = getelementptr  %program, %program* %arg_0, i32 0, i32 4
         call ccc  void  @btree_swap_0(%btree_t_0*  %66, %btree_t_0*  %67)
        br label %loop_0
      loop.end:
        %value_15_0 = alloca %value_t_0, i32 1
        %68 = getelementptr  %value_t_0, %value_t_0* %value_15_0, i32 0, i32 0
        store   i32 0, %column_t_0* %68
        %value_16_0 = alloca %value_t_0, i32 1
        %69 = getelementptr  %value_t_0, %value_t_0* %value_16_0, i32 0, i32 0
        store   i32 4294967295, %column_t_0* %69
        %begin_iter_8_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_8_0 = alloca %btree_iterator_t_0, i32 1
        %70 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %70, %value_t_0*  %value_15_0, %btree_iterator_t_0*  %begin_iter_8_0)
        %71 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %71, %value_t_0*  %value_16_0, %btree_iterator_t_0*  %end_iter_8_0)
        br label %loop_5
      loop_5:
        %condition_8_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_8_0, %btree_iterator_t_0*  %end_iter_8_0)
        br i1 %condition_8_0, label %if_8, label %end_if_8
      if_8:
        br label %range_query.end_4
      end_if_8:
        %current_4_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_8_0)
        %value_17_0 = alloca %value_t_0, i32 1
        %72 = getelementptr  %value_t_0, %value_t_0* %value_17_0, i32 0, i32 0
        %73 = getelementptr  %value_t_0, %value_t_0* %current_4_0, i32 0, i32 0
        %74 = load   %column_t_0, %column_t_0* %73
        store   %column_t_0 %74, %column_t_0* %72
        %value_18_0 = alloca %value_t_0, i32 1
        %75 = getelementptr  %value_t_0, %value_t_0* %value_18_0, i32 0, i32 0
        %76 = getelementptr  %value_t_0, %value_t_0* %current_4_0, i32 0, i32 0
        %77 = load   %column_t_0, %column_t_0* %76
        store   %column_t_0 %77, %column_t_0* %75
        %begin_iter_9_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_9_0 = alloca %btree_iterator_t_0, i32 1
        %78 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %78, %value_t_0*  %value_17_0, %btree_iterator_t_0*  %begin_iter_9_0)
        %79 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %79, %value_t_0*  %value_18_0, %btree_iterator_t_0*  %end_iter_9_0)
        br label %loop_6
      loop_6:
        %condition_9_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_9_0, %btree_iterator_t_0*  %end_iter_9_0)
        br i1 %condition_9_0, label %if_9, label %end_if_9
      if_9:
        br label %range_query.end_5
      end_if_9:
        %current_5_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_9_0)
        %value_19_0 = alloca %value_t_0, i32 1
        %80 = getelementptr  %value_t_0, %value_t_0* %value_19_0, i32 0, i32 0
        %81 = getelementptr  %value_t_0, %value_t_0* %current_4_0, i32 0, i32 0
        %82 = load   %column_t_0, %column_t_0* %81
        store   %column_t_0 %82, %column_t_0* %80
        %83 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
        %84 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %83, %value_t_0*  %value_19_0)
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_9_0)
        br label %loop_6
      range_query.end_5:
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_8_0)
        br label %loop_5
      range_query.end_4:
        ret void
      }
      |]
  -- TODO tests for rules with >2 clauses, ...

  it "can generate code for program with no top level facts" $ do
    llvmIR <- cg "no_top_level_facts"
    extractDeclTypeSnippet llvmIR `shouldBe` "%program = type {%btree_t_0, %btree_t_0, %btree_t_0, %btree_t_0}"
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      define external ccc  %program* @eclair_program_init()    {
        %byte_count_0 = trunc i64 ptrtoint (%program* getelementptr inbounds (%program, %program* inttoptr (i64 0 to %program*), i64 1) to i64) to i32
        %memory_0 =  call ccc  i8*  @malloc(i32  %byte_count_0)
        %program_0 = bitcast i8* %memory_0 to %program*
        %1 = getelementptr  %program, %program* %program_0, i32 0, i32 0
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %program_0, i32 0, i32 1
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %2)
        %3 = getelementptr  %program, %program* %program_0, i32 0, i32 2
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %3)
        %4 = getelementptr  %program, %program* %program_0, i32 0, i32 3
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %4)
        ret %program* %program_0
      }
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      define external ccc  void @eclair_program_destroy(%program*  %arg_0)    {
        %1 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_destroy_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_destroy_0(%btree_t_0*  %2)
        %3 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_destroy_0(%btree_t_0*  %3)
        %4 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_destroy_0(%btree_t_0*  %4)
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
        store   i32 0, %column_t_0* %1
        %2 = getelementptr  %value_t_0, %value_t_0* %value_0, i32 0, i32 1
        store   i32 0, %column_t_0* %2
        %value_1_0 = alloca %value_t_0, i32 1
        %3 = getelementptr  %value_t_0, %value_t_0* %value_1_0, i32 0, i32 0
        store   i32 4294967295, %column_t_0* %3
        %4 = getelementptr  %value_t_0, %value_t_0* %value_1_0, i32 0, i32 1
        store   i32 4294967295, %column_t_0* %4
        %begin_iter_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_0 = alloca %btree_iterator_t_0, i32 1
        %5 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %5, %value_t_0*  %value_0, %btree_iterator_t_0*  %begin_iter_0)
        %6 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %6, %value_t_0*  %value_1_0, %btree_iterator_t_0*  %end_iter_0)
        br label %loop_0
      loop_0:
        %condition_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_0, %btree_iterator_t_0*  %end_iter_0)
        br i1 %condition_0, label %if_0, label %end_if_0
      if_0:
        br label %range_query.end
      end_if_0:
        %current_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_0)
        %value_2_0 = alloca %value_t_0, i32 1
        %7 = getelementptr  %value_t_0, %value_t_0* %value_2_0, i32 0, i32 0
        %8 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 0
        %9 = load   %column_t_0, %column_t_0* %8
        store   %column_t_0 %9, %column_t_0* %7
        %10 = getelementptr  %value_t_0, %value_t_0* %value_2_0, i32 0, i32 1
        %11 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 1
        %12 = load   %column_t_0, %column_t_0* %11
        store   %column_t_0 %12, %column_t_0* %10
        %13 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
        %14 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %13, %value_t_0*  %value_2_0)
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_0)
        br label %loop_0
      range_query.end:
        %begin_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_1_0 = alloca %btree_iterator_t_0, i32 1
        %15 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_begin_0(%btree_t_0*  %15, %btree_iterator_t_0*  %begin_iter_1_0)
        %16 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_end_0(%btree_t_0*  %16, %btree_iterator_t_0*  %end_iter_1_0)
        %17 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_insert_range_0(%btree_t_0*  %17, %btree_iterator_t_0*  %begin_iter_1_0, %btree_iterator_t_0*  %end_iter_1_0)
        br label %loop_1
      loop_1:
        %18 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_clear_0(%btree_t_0*  %18)
        %value_3_0 = alloca %value_t_0, i32 1
        %19 = getelementptr  %value_t_0, %value_t_0* %value_3_0, i32 0, i32 0
        store   i32 0, %column_t_0* %19
        %20 = getelementptr  %value_t_0, %value_t_0* %value_3_0, i32 0, i32 1
        store   i32 0, %column_t_0* %20
        %value_4_0 = alloca %value_t_0, i32 1
        %21 = getelementptr  %value_t_0, %value_t_0* %value_4_0, i32 0, i32 0
        store   i32 4294967295, %column_t_0* %21
        %22 = getelementptr  %value_t_0, %value_t_0* %value_4_0, i32 0, i32 1
        store   i32 4294967295, %column_t_0* %22
        %begin_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_2_0 = alloca %btree_iterator_t_0, i32 1
        %23 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %23, %value_t_0*  %value_3_0, %btree_iterator_t_0*  %begin_iter_2_0)
        %24 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %24, %value_t_0*  %value_4_0, %btree_iterator_t_0*  %end_iter_2_0)
        br label %loop_2
      loop_2:
        %condition_1_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_2_0, %btree_iterator_t_0*  %end_iter_2_0)
        br i1 %condition_1_0, label %if_1, label %end_if_1
      if_1:
        br label %range_query.end_1
      end_if_1:
        %current_1_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_2_0)
        %value_5_0 = alloca %value_t_0, i32 1
        %25 = getelementptr  %value_t_0, %value_t_0* %value_5_0, i32 0, i32 0
        %26 = getelementptr  %value_t_0, %value_t_0* %current_1_0, i32 0, i32 1
        %27 = load   %column_t_0, %column_t_0* %26
        store   %column_t_0 %27, %column_t_0* %25
        %28 = getelementptr  %value_t_0, %value_t_0* %value_5_0, i32 0, i32 1
        store   i32 0, %column_t_0* %28
        %value_6_0 = alloca %value_t_0, i32 1
        %29 = getelementptr  %value_t_0, %value_t_0* %value_6_0, i32 0, i32 0
        %30 = getelementptr  %value_t_0, %value_t_0* %current_1_0, i32 0, i32 1
        %31 = load   %column_t_0, %column_t_0* %30
        store   %column_t_0 %31, %column_t_0* %29
        %32 = getelementptr  %value_t_0, %value_t_0* %value_6_0, i32 0, i32 1
        store   i32 4294967295, %column_t_0* %32
        %begin_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_3_0 = alloca %btree_iterator_t_0, i32 1
        %33 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_lower_bound_0(%btree_t_0*  %33, %value_t_0*  %value_5_0, %btree_iterator_t_0*  %begin_iter_3_0)
        %34 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_upper_bound_0(%btree_t_0*  %34, %value_t_0*  %value_6_0, %btree_iterator_t_0*  %end_iter_3_0)
        br label %loop_3
      loop_3:
        %condition_2_0 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %begin_iter_3_0, %btree_iterator_t_0*  %end_iter_3_0)
        br i1 %condition_2_0, label %if_2, label %end_if_2
      if_2:
        br label %range_query.end_2
      end_if_2:
        %current_2_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %begin_iter_3_0)
        %value_7_0 = alloca %value_t_0, i32 1
        %35 = getelementptr  %value_t_0, %value_t_0* %value_7_0, i32 0, i32 0
        %36 = getelementptr  %value_t_0, %value_t_0* %current_1_0, i32 0, i32 0
        %37 = load   %column_t_0, %column_t_0* %36
        store   %column_t_0 %37, %column_t_0* %35
        %38 = getelementptr  %value_t_0, %value_t_0* %value_7_0, i32 0, i32 1
        %39 = getelementptr  %value_t_0, %value_t_0* %current_2_0, i32 0, i32 1
        %40 = load   %column_t_0, %column_t_0* %39
        store   %column_t_0 %40, %column_t_0* %38
        %contains_result_0 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
        %contains_result_1 =  call ccc  i1  @btree_contains_0(%btree_t_0*  %contains_result_0, %value_t_0*  %value_7_0)
        %condition_3_0 = select i1 %contains_result_1, i1 0, i1 1
        br i1 %condition_3_0, label %if_3, label %end_if_3
      if_3:
        %value_8_0 = alloca %value_t_0, i32 1
        %41 = getelementptr  %value_t_0, %value_t_0* %value_8_0, i32 0, i32 0
        %42 = getelementptr  %value_t_0, %value_t_0* %current_1_0, i32 0, i32 0
        %43 = load   %column_t_0, %column_t_0* %42
        store   %column_t_0 %43, %column_t_0* %41
        %44 = getelementptr  %value_t_0, %value_t_0* %value_8_0, i32 0, i32 1
        %45 = getelementptr  %value_t_0, %value_t_0* %current_2_0, i32 0, i32 1
        %46 = load   %column_t_0, %column_t_0* %45
        store   %column_t_0 %46, %column_t_0* %44
        %47 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
        %48 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %47, %value_t_0*  %value_8_0)
        br label %end_if_3
      end_if_3:
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_3_0)
        br label %loop_3
      range_query.end_2:
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_2_0)
        br label %loop_2
      range_query.end_1:
        %condition_4_0 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
        %condition_4_1 =  call ccc  i1  @btree_is_empty_0(%btree_t_0*  %condition_4_0)
        br i1 %condition_4_1, label %if_4, label %end_if_4
      if_4:
        br label %loop.end
      end_if_4:
        %begin_iter_4_0 = alloca %btree_iterator_t_0, i32 1
        %end_iter_4_0 = alloca %btree_iterator_t_0, i32 1
        %49 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_begin_0(%btree_t_0*  %49, %btree_iterator_t_0*  %begin_iter_4_0)
        %50 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
         call ccc  void  @btree_end_0(%btree_t_0*  %50, %btree_iterator_t_0*  %end_iter_4_0)
        %51 = getelementptr  %program, %program* %arg_0, i32 0, i32 3
         call ccc  void  @btree_insert_range_0(%btree_t_0*  %51, %btree_iterator_t_0*  %begin_iter_4_0, %btree_iterator_t_0*  %end_iter_4_0)
        %52 = getelementptr  %program, %program* %arg_0, i32 0, i32 2
        %53 = getelementptr  %program, %program* %arg_0, i32 0, i32 0
         call ccc  void  @btree_swap_0(%btree_t_0*  %52, %btree_t_0*  %53)
        br label %loop_1
      loop.end:
        ret void
      }
      |]

  describe "fact IO" $ parallel $ do
    it "generates valid code for empty programs" $ do
      llvmIR <- cg "empty"
      extractFnSnippet llvmIR "eclair_add_fact" `shouldBe` Just [text|
        define external ccc  void @eclair_add_fact(%program*  %eclair_program_0, i16  %fact_type_0, i32*  %memory_0)    {
           call ccc  void  @eclair_add_facts(%program*  %eclair_program_0, i16  %fact_type_0, i32*  %memory_0, i32  1)
          ret void
        }
        |]
      extractFnSnippet llvmIR "eclair_add_facts" `shouldBe` Just [text|
        define external ccc  void @eclair_add_facts(%program*  %eclair_program_0, i16  %fact_type_0, i32*  %memory_0, i32  %fact_count_0)    {
        ; <label>:0:
          switch i16 %fact_type_0, label %switch.default_0 []
        switch.default_0:
          ret void
        }
        |]
      extractFnSnippet llvmIR "eclair_get_facts" `shouldBe` Just [text|
        define external ccc  i32* @eclair_get_facts(%program*  %eclair_program_0, i16  %fact_type_0)    {
        ; <label>:0:
          switch i16 %fact_type_0, label %switch.default_0 []
        switch.default_0:
          ret i32* zeroinitializer
        }
        |]

    it "only generates IO code for relations visible to the user" $ do
      llvmIR <- cg "no_top_level_facts"
      extractFnSnippet llvmIR "eclair_add_facts" `shouldBe` Just [text|
        define external ccc  void @eclair_add_facts(%program*  %eclair_program_0, i16  %fact_type_0, i32*  %memory_0, i32  %fact_count_0)    {
        ; <label>:0:
          switch i16 %fact_type_0, label %switch.default_0 [i16 0, label %edge_0 i16 1, label %path_0]
        edge_0:
          %1 = getelementptr  %program, %program* %eclair_program_0, i32 0, i32 1
          %2 = bitcast i32* %memory_0 to [2 x i32]*
          br label %for_begin_0
        for_begin_0:
          %3 = phi i32 [0, %edge_0], [%7, %for_body_0]
          %4 = icmp ult i32 %3, %fact_count_0
          br i1 %4, label %for_body_0, label %for_end_0
        for_body_0:
          %5 = getelementptr  [2 x i32], [2 x i32]* %2, i32 %3
          %6 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %1, [2 x i32]*  %5)
          %7 = add   i32 1, %3
          br label %for_begin_0
        for_end_0:
          ret void
        path_0:
          %8 = getelementptr  %program, %program* %eclair_program_0, i32 0, i32 3
          %9 = bitcast i32* %memory_0 to [2 x i32]*
          br label %for_begin_1
        for_begin_1:
          %10 = phi i32 [0, %path_0], [%14, %for_body_1]
          %11 = icmp ult i32 %10, %fact_count_0
          br i1 %11, label %for_body_1, label %for_end_1
        for_body_1:
          %12 = getelementptr  [2 x i32], [2 x i32]* %9, i32 %10
          %13 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %8, [2 x i32]*  %12)
          %14 = add   i32 1, %10
          br label %for_begin_1
        for_end_1:
          ret void
        switch.default_0:
          ret void
        }
        |]
      extractFnSnippet llvmIR "eclair_get_facts" `shouldBe` Just [text|
        define external ccc  i32* @eclair_get_facts(%program*  %eclair_program_0, i16  %fact_type_0)    {
        ; <label>:0:
          switch i16 %fact_type_0, label %switch.default_0 [i16 0, label %edge_0 i16 1, label %path_0]
        edge_0:
          %1 = getelementptr  %program, %program* %eclair_program_0, i32 0, i32 1
          %fact_count_0 =  call ccc  i64  @btree_size_0(%btree_t_0*  %1)
          %fact_count_1 = trunc i64 %fact_count_0 to i32
          %byte_count_0 = mul   i32 %fact_count_1, 8
          %memory_0 =  call ccc  i8*  @malloc(i32  %byte_count_0)
          %array_0 = bitcast i8* %memory_0 to [2 x i32]*
          %i_0 = alloca i32, i32 1
          store   i32 0, i32* %i_0
          %current_iter_0 = alloca %btree_iterator_t_0, i32 1
          %end_iter_0 = alloca %btree_iterator_t_0, i32 1
           call ccc  void  @btree_begin_0(%btree_t_0*  %1, %btree_iterator_t_0*  %current_iter_0)
           call ccc  void  @btree_end_0(%btree_t_0*  %1, %btree_iterator_t_0*  %end_iter_0)
          br label %while_begin_0
        while_begin_0:
          %2 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %current_iter_0, %btree_iterator_t_0*  %end_iter_0)
          %3 = select i1 %2, i1 0, i1 1
          br i1 %3, label %while_body_0, label %while_end_0
        while_body_0:
          %4 = load   i32, i32* %i_0
          %value_0 = getelementptr  [2 x i32], [2 x i32]* %array_0, i32 %4
          %current_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %current_iter_0)
          %5 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0
          %6 = load   %value_t_0, %value_t_0* %5
          %7 = getelementptr  [2 x i32], [2 x i32]* %value_0, i32 0
          store   %value_t_0 %6, [2 x i32]* %7
          %8 = add   i32 %4, 1
          store   i32 %8, i32* %i_0
           call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %current_iter_0)
          br label %while_begin_0
        while_end_0:
          %9 = bitcast i8* %memory_0 to i32*
          ret i32* %9
        path_0:
          %10 = getelementptr  %program, %program* %eclair_program_0, i32 0, i32 3
          %fact_count_2 =  call ccc  i64  @btree_size_0(%btree_t_0*  %10)
          %fact_count_3 = trunc i64 %fact_count_2 to i32
          %byte_count_1 = mul   i32 %fact_count_3, 8
          %memory_1 =  call ccc  i8*  @malloc(i32  %byte_count_1)
          %array_1 = bitcast i8* %memory_1 to [2 x i32]*
          %i_1 = alloca i32, i32 1
          store   i32 0, i32* %i_1
          %current_iter_1 = alloca %btree_iterator_t_0, i32 1
          %end_iter_1 = alloca %btree_iterator_t_0, i32 1
           call ccc  void  @btree_begin_0(%btree_t_0*  %10, %btree_iterator_t_0*  %current_iter_1)
           call ccc  void  @btree_end_0(%btree_t_0*  %10, %btree_iterator_t_0*  %end_iter_1)
          br label %while_begin_1
        while_begin_1:
          %11 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %current_iter_1, %btree_iterator_t_0*  %end_iter_1)
          %12 = select i1 %11, i1 0, i1 1
          br i1 %12, label %while_body_1, label %while_end_1
        while_body_1:
          %13 = load   i32, i32* %i_1
          %value_1 = getelementptr  [2 x i32], [2 x i32]* %array_1, i32 %13
          %current_1 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %current_iter_1)
          %14 = getelementptr  %value_t_0, %value_t_0* %current_1, i32 0
          %15 = load   %value_t_0, %value_t_0* %14
          %16 = getelementptr  [2 x i32], [2 x i32]* %value_1, i32 0
          store   %value_t_0 %15, [2 x i32]* %16
          %17 = add   i32 %13, 1
          store   i32 %17, i32* %i_1
           call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %current_iter_1)
          br label %while_begin_1
        while_end_1:
          %18 = bitcast i8* %memory_1 to i32*
          ret i32* %18
        switch.default_0:
          ret i32* zeroinitializer
        }
        |]

    it "generates correct code with facts of different types" $ do
      llvmIR <- cg "different_types"
      extractFnSnippet llvmIR "eclair_add_facts" `shouldBe` Just [text|
        define external ccc  void @eclair_add_facts(%program*  %eclair_program_0, i16  %fact_type_0, i32*  %memory_0, i32  %fact_count_0)    {
        ; <label>:0:
          switch i16 %fact_type_0, label %switch.default_0 [i16 0, label %a_0 i16 1, label %b_0]
        a_0:
          %1 = getelementptr  %program, %program* %eclair_program_0, i32 0, i32 0
          %2 = bitcast i32* %memory_0 to [1 x i32]*
          br label %for_begin_0
        for_begin_0:
          %3 = phi i32 [0, %a_0], [%7, %for_body_0]
          %4 = icmp ult i32 %3, %fact_count_0
          br i1 %4, label %for_body_0, label %for_end_0
        for_body_0:
          %5 = getelementptr  [1 x i32], [1 x i32]* %2, i32 %3
          %6 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %1, [1 x i32]*  %5)
          %7 = add   i32 1, %3
          br label %for_begin_0
        for_end_0:
          ret void
        b_0:
          %8 = getelementptr  %program, %program* %eclair_program_0, i32 0, i32 1
          %9 = bitcast i32* %memory_0 to [3 x i32]*
          br label %for_begin_1
        for_begin_1:
          %10 = phi i32 [0, %b_0], [%14, %for_body_1]
          %11 = icmp ult i32 %10, %fact_count_0
          br i1 %11, label %for_body_1, label %for_end_1
        for_body_1:
          %12 = getelementptr  [3 x i32], [3 x i32]* %9, i32 %10
          %13 =  call ccc  i1  @btree_insert_value_1(%btree_t_1*  %8, [3 x i32]*  %12)
          %14 = add   i32 1, %10
          br label %for_begin_1
        for_end_1:
          ret void
        switch.default_0:
          ret void
        }
        |]
      extractFnSnippet llvmIR "eclair_get_facts" `shouldBe` Just [text|
        define external ccc  i32* @eclair_get_facts(%program*  %eclair_program_0, i16  %fact_type_0)    {
        ; <label>:0:
          switch i16 %fact_type_0, label %switch.default_0 [i16 0, label %a_0 i16 1, label %b_0]
        a_0:
          %1 = getelementptr  %program, %program* %eclair_program_0, i32 0, i32 0
          %fact_count_0 =  call ccc  i64  @btree_size_0(%btree_t_0*  %1)
          %fact_count_1 = trunc i64 %fact_count_0 to i32
          %byte_count_0 = mul   i32 %fact_count_1, 4
          %memory_0 =  call ccc  i8*  @malloc(i32  %byte_count_0)
          %array_0 = bitcast i8* %memory_0 to [1 x i32]*
          %i_0 = alloca i32, i32 1
          store   i32 0, i32* %i_0
          %current_iter_0 = alloca %btree_iterator_t_0, i32 1
          %end_iter_0 = alloca %btree_iterator_t_0, i32 1
           call ccc  void  @btree_begin_0(%btree_t_0*  %1, %btree_iterator_t_0*  %current_iter_0)
           call ccc  void  @btree_end_0(%btree_t_0*  %1, %btree_iterator_t_0*  %end_iter_0)
          br label %while_begin_0
        while_begin_0:
          %2 =  call ccc  i1  @btree_iterator_is_equal_0(%btree_iterator_t_0*  %current_iter_0, %btree_iterator_t_0*  %end_iter_0)
          %3 = select i1 %2, i1 0, i1 1
          br i1 %3, label %while_body_0, label %while_end_0
        while_body_0:
          %4 = load   i32, i32* %i_0
          %value_0 = getelementptr  [1 x i32], [1 x i32]* %array_0, i32 %4
          %current_0 =  call ccc  %value_t_0*  @btree_iterator_current_0(%btree_iterator_t_0*  %current_iter_0)
          %5 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0
          %6 = load   %value_t_0, %value_t_0* %5
          %7 = getelementptr  [1 x i32], [1 x i32]* %value_0, i32 0
          store   %value_t_0 %6, [1 x i32]* %7
          %8 = add   i32 %4, 1
          store   i32 %8, i32* %i_0
           call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %current_iter_0)
          br label %while_begin_0
        while_end_0:
          %9 = bitcast i8* %memory_0 to i32*
          ret i32* %9
        b_0:
          %10 = getelementptr  %program, %program* %eclair_program_0, i32 0, i32 1
          %fact_count_2 =  call ccc  i64  @btree_size_1(%btree_t_1*  %10)
          %fact_count_3 = trunc i64 %fact_count_2 to i32
          %byte_count_1 = mul   i32 %fact_count_3, 12
          %memory_1 =  call ccc  i8*  @malloc(i32  %byte_count_1)
          %array_1 = bitcast i8* %memory_1 to [3 x i32]*
          %i_1 = alloca i32, i32 1
          store   i32 0, i32* %i_1
          %current_iter_1 = alloca %btree_iterator_t_1, i32 1
          %end_iter_1 = alloca %btree_iterator_t_1, i32 1
           call ccc  void  @btree_begin_1(%btree_t_1*  %10, %btree_iterator_t_1*  %current_iter_1)
           call ccc  void  @btree_end_1(%btree_t_1*  %10, %btree_iterator_t_1*  %end_iter_1)
          br label %while_begin_1
        while_begin_1:
          %11 =  call ccc  i1  @btree_iterator_is_equal_1(%btree_iterator_t_1*  %current_iter_1, %btree_iterator_t_1*  %end_iter_1)
          %12 = select i1 %11, i1 0, i1 1
          br i1 %12, label %while_body_1, label %while_end_1
        while_body_1:
          %13 = load   i32, i32* %i_1
          %value_1 = getelementptr  [3 x i32], [3 x i32]* %array_1, i32 %13
          %current_1 =  call ccc  %value_t_1*  @btree_iterator_current_1(%btree_iterator_t_1*  %current_iter_1)
          %14 = getelementptr  %value_t_1, %value_t_1* %current_1, i32 0
          %15 = load   %value_t_1, %value_t_1* %14
          %16 = getelementptr  [3 x i32], [3 x i32]* %value_1, i32 0
          store   %value_t_1 %15, [3 x i32]* %16
          %17 = add   i32 %13, 1
          store   i32 %17, i32* %i_1
           call ccc  void  @btree_iterator_next_1(%btree_iterator_t_1*  %current_iter_1)
          br label %while_begin_1
        while_end_1:
          %18 = bitcast i8* %memory_1 to i32*
          ret i32* %18
        switch.default_0:
          ret i32* zeroinitializer
        }
        |]
