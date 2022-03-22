{-# LANGUAGE TypeFamilies, RankNTypes, QuasiQuotes #-}

module Test.Eclair.EIR.CodegenSpec
  ( module Test.Eclair.EIR.CodegenSpec
  ) where

import qualified Data.Text as T
import Eclair
import qualified Eclair.EIR.IR as EIR
import Eclair.EIR.Printer
import Eclair.Syntax
import Protolude hiding ((<.>))
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


spec :: Spec
spec = describe "EIR Code Generation" $ parallel $ do
  fit "generates code for a single fact" $ do
    cg "single_fact" `resultsIn` [text|
      {
        declare_type Program
        {
          btree(num_columns=3, index=[0, 1, 2], block_size=256, search_type=linear)
          btree(num_columns=2, index=[0, 1], block_size=256, search_type=linear)
        }
        fn eclair_program_init()
        {
          {
            program = heap_allocate (Program)
            {
              init_empty(program.0)
              init_empty(program.1)
            }
            return program
          }
        }
        fn eclair_program_destroy(*Program)
        {
          {
            {
              destroy(FN_ARG.0.0)
              destroy(FN_ARG.0.1)
            }
            free(FN_ARG.0)
          }
        }
        fn eclair_program_run(*Program)
        {
          {
            {
              {
                value = stack_allocate Value another
                value.0 = 1
                value.1 = 2
                value.2 = 3
                insert(FN_ARG.0.0, value)
              }
              {
                value = stack_allocate Value edge
                value.0 = 2
                value.1 = 3
                insert(FN_ARG.0.1, value)
              }
              {
                value = stack_allocate Value edge
                value.0 = 1
                value.1 = 2
                insert(FN_ARG.0.1, value)
              }
            }
            goto the.end
            the.end:
          }
        }
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
