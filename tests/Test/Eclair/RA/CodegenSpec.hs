
{-# LANGUAGE DeriveGeneric, TypeFamilies, DataKinds, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Test.Eclair.RA.CodegenSpec
  ( module Test.Eclair.RA.CodegenSpec
  ) where

import qualified Data.Text as T
import Eclair
import Eclair.RA.IR
import Eclair.Syntax
import Protolude hiding ((<.>))
import System.FilePath
import Test.Hspec

cg :: FilePath -> IO RA
cg path = do
  let file = "tests/fixtures/codegen" </> path <.> "dl"
  result <- compile file
  case result of
    Left err -> panic $ "Failed to parse " <> T.pack file <> "!"
    Right ra -> pure ra

spec :: Spec
spec = describe "RA Code Generation" $ parallel $ do
  it "generates code for a single fact" $ do
    output <- cg "single_fact"
    output `shouldBe`
      RAModule [ Project (Id "another") [RALit 1, RALit 2, RALit 3]
               , Project (Id "edge") [RALit 2, RALit 3]
               , Project (Id "edge") [RALit 1, RALit 2]
               ]

  it "generates code for a single non-recursive rule" $ do
    output <- cg "single_nonrecursive_rule"
    output `shouldBe`
      RAModule [ Project (Id "edge") [RALit 1, RALit 2]
               , Search (Id "edge") []
                    (Project (Id "path")
                      [ ColumnIndex (Id "edge") 0
                      , ColumnIndex (Id "edge") 1
                      ])
               ]

  it "generates nested searches correctly" $ do
    pending

  it "generates code for a rule with 2 clauses of same name" $ do
    -- TODO chain example
    pending


  it "generates code for a single recursive rule" $ do
    pending

  it "generates code for mutually recursive rules" $ do
    pending

  it "generates code for multiple dependent rules" $ do
    pending
