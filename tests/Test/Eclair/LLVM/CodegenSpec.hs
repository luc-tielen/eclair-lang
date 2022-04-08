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
        %program_0 =  call ccc  i8*  @malloc(i64  ptrtoint (%program* getelementptr inbounds (%program, %program* inttoptr (i64 0 to %program*), i64 1) to i64))
        %program_1 = bitcast i8* %program_0 to %program*
        %1 = getelementptr  %program, %program* %program_1, i32 0, i32 0
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %program_1, i32 0, i32 1
         call ccc  void  @btree_init_empty_1(%btree_t_1*  %2)
        ret %program* %program_1
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
        %program_0 =  call ccc  i8*  @malloc(i64  ptrtoint (%program* getelementptr inbounds (%program, %program* inttoptr (i64 0 to %program*), i64 1) to i64))
        %program_1 = bitcast i8* %program_0 to %program*
        %1 = getelementptr  %program, %program* %program_1, i32 0, i32 0
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %1)
        %2 = getelementptr  %program, %program* %program_1, i32 0, i32 1
         call ccc  void  @btree_init_empty_0(%btree_t_0*  %2)
        ret %program* %program_1
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
        store   %column_t_0* %12, %column_t_0* %11
        %13 = getelementptr  %value_t_0, %value_t_0* %value_3_0, i32 0, i32 1
        %14 = getelementptr  %value_t_0, %value_t_0* %current_0, i32 0, i32 1
        store   %column_t_0* %14, %column_t_0* %13
        %15 = getelementptr  %program, %program* %arg_0, i32 0, i32 1
        %16 =  call ccc  i1  @btree_insert_value_0(%btree_t_0*  %15, %value_t_0*  %value_3_0)
         call ccc  void  @btree_iterator_next_0(%btree_iterator_t_0*  %begin_iter_0)
        br label %loop_0
      range_query.end:
        ret void
      }
      |]

  {-
  it "generates nested searches correctly" $ do
    llvmIR <- cg "multiple_rule_clauses"
    extractDeclTypeSnippet llvmIR `shouldBe` [text|
      |]
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      |]

  it "generates code for a rule with 2 clauses of same name" $ do
    llvmIR <- cg "multiple_clauses_same_name"
    extractDeclTypeSnippet llvmIR `shouldBe` [text|
      |]
    extractFnSnippet llvmIR "eclair_program_init" `shouldBe` Just [text|
      |]
    extractFnSnippet llvmIR "eclair_program_destroy" `shouldBe` Just [text|
      |]
    extractFnSnippet llvmIR "eclair_program_run" `shouldBe` Just [text|
      |]

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
