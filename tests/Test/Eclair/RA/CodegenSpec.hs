
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
               , Search (Id "edge") (Id "edge0") []
                    (Project (Id "path")
                      [ ColumnIndex (Id "edge0") 0
                      , ColumnIndex (Id "edge0") 1
                      ])
               ]

  it "generates nested searches correctly" $ do
    output <- cg "multiple_rule_clauses"
    output `shouldBe`
      RAModule
        [ Project (Id "second") [RALit 2, RALit 3]
        , Project (Id "first") [RALit 1]
        , Search (Id "first") (Id "first0") []
            (Search (Id "second") (Id "second1")
              [RAConstraint
                (ColumnIndex (Id "second1") 1)
                (ColumnIndex (Id "first0") 0)]
              (Project (Id "third") [ ColumnIndex (Id "second1") 0
                                    , ColumnIndex (Id "first0") 0]))
                                    ]

  it "generates code for a rule with 2 clauses of same name" $ do
    output <- cg "multiple_clauses_same_name"
    output `shouldBe`
      RAModule
        [ Project (Id "link") [RALit 1, RALit 2]
        , Search (Id "link") (Id "link0") []
            (Search (Id "link") (Id "link1") [RAConstraint
                                  (ColumnIndex (Id "link1") 0)
                                  (ColumnIndex (Id "link0") 1)]
              (Project (Id "chain") [ ColumnIndex (Id "link0") 0
                                    , ColumnIndex (Id "link0") 1
                                    , ColumnIndex (Id "link1") 1]))
                                    ]

  it "generates code for a single recursive rule" $ do
    output <- cg "single_recursive_rule"
    output `shouldBe`
      RAModule
        [ Project (Id "edge") [RALit 1,RALit 2]

        , Merge (Id "path") (Id "delta_path")
        , Loop (Seq
          [ Purge (Id "new_path")
          , Search (Id "edge") (Id "edge0") []
              (Search (Id "delta_path") (Id "delta_path1")
                [RAConstraint (ColumnIndex (Id "delta_path1") 0)
                              (ColumnIndex (Id "edge0") 1)
                -- TODO: check that tuple isn't in 'path' yet
                ]
                (Project (Id "new_path") [ ColumnIndex (Id "edge0") 0
                                         , ColumnIndex (Id "delta_path1") 1]))
          , Exit [Id "new_path"]
          , Merge (Id "new_path") (Id "path")
          , Swap (Id "new_path") (Id "delta_path")
          ])
        ]

  -- TODO variant where one is recursive
  it "generates code for mutually recursive rules" $ do
    pending

  it "generates code for multiple dependent rules" $ do
    pending

  -- TODO tests for lits, rules with >2 clauses, ...
