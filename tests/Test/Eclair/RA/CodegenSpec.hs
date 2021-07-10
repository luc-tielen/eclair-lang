
{-# LANGUAGE DeriveGeneric, TypeFamilies, DataKinds, RankNTypes #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Test.Eclair.RA.CodegenSpec
  ( module Test.Eclair.RA.CodegenSpec
  ) where

import qualified Data.Text as T
import Eclair
import qualified Eclair.RA.IR as RA
import Eclair.Syntax
import Protolude hiding ((<.>))
import System.FilePath
import Test.Hspec

cg :: FilePath -> IO RA.RA
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
      RA.Module [ RA.Project (Id "another") [RA.Lit 1, RA.Lit 2, RA.Lit 3]
                , RA.Project (Id "edge") [RA.Lit 2, RA.Lit 3]
                , RA.Project (Id "edge") [RA.Lit 1, RA.Lit 2]
                ]

  it "generates code for a single non-recursive rule" $ do
    output <- cg "single_nonrecursive_rule"
    output `shouldBe`
      RA.Module [ RA.Project (Id "edge") [RA.Lit 1, RA.Lit 2]
               , RA.Search (Id "edge") (Id "edge0") []
                    (RA.Project (Id "path")
                      [ RA.ColumnIndex (Id "edge0") 0
                      , RA.ColumnIndex (Id "edge0") 1
                      ])
               ]

  it "generates nested searches correctly" $ do
    output <- cg "multiple_rule_clauses"
    output `shouldBe`
      RA.Module
        [ RA.Project (Id "second") [RA.Lit 2, RA.Lit 3]
        , RA.Project (Id "first") [RA.Lit 1]
        , RA.Search (Id "first") (Id "first0") []
            (RA.Search (Id "second") (Id "second1")
              [RA.Constrain
                (RA.ColumnIndex (Id "second1") 1)
                (RA.ColumnIndex (Id "first0") 0)]
              (RA.Project (Id "third") [ RA.ColumnIndex (Id "second1") 0
                                    , RA.ColumnIndex (Id "first0") 0]))
                                    ]

  it "generates code for a rule with 2 clauses of same name" $ do
    output <- cg "multiple_clauses_same_name"
    output `shouldBe`
      RA.Module
        [ RA.Project (Id "link") [RA.Lit 1, RA.Lit 2]
        , RA.Search (Id "link") (Id "link0") []
            (RA.Search (Id "link") (Id "link1") [RA.Constrain
                                  (RA.ColumnIndex (Id "link1") 0)
                                  (RA.ColumnIndex (Id "link0") 1)]
              (RA.Project (Id "chain") [ RA.ColumnIndex (Id "link0") 0
                                    , RA.ColumnIndex (Id "link0") 1
                                    , RA.ColumnIndex (Id "link1") 1]))
                                    ]

  it "generates code for a single recursive rule" $ do
    output <- cg "single_recursive_rule"
    output `shouldBe`
      RA.Module
        [ RA.Project (Id "edge") [RA.Lit 1,RA.Lit 2]

        , RA.Merge (Id "path") (Id "delta_path")
        , RA.Loop (RA.Seq
          [ RA.Purge (Id "new_path")
          , RA.Search (Id "edge") (Id "edge0") []
              (RA.Search (Id "delta_path") (Id "delta_path1")
                [RA.Constrain (RA.ColumnIndex (Id "delta_path1") 0)
                              (RA.ColumnIndex (Id "edge0") 1)
                , RA.NotElem (Id "path") [ RA.ColumnIndex (Id "edge0") 0
                                         , RA.ColumnIndex (Id "delta_path1") 1]
                ]
                (RA.Project (Id "new_path") [ RA.ColumnIndex (Id "edge0") 0
                                         , RA.ColumnIndex (Id "delta_path1") 1]))
          , RA.Exit [Id "new_path"]
          , RA.Merge (Id "new_path") (Id "path")
          , RA.Swap (Id "new_path") (Id "delta_path")
          ])
        ]

  -- TODO variant where one is recursive
  it "generates code for mutually recursive rules" $ do
    output <- cg "mutually_recursive_rules"
    output `shouldBe`
      RA.Module
        [ RA.Project (Id "d") [RA.Lit 3]
        , RA.Project (Id "c") [RA.Lit 2]
        , RA.Project (Id "b") [RA.Lit 1]
        , RA.Merge (Id "c") (Id "delta_c")
        , RA.Merge (Id "b") (Id "delta_b")

        , RA.Loop (RA.Seq
          [ RA.Purge (Id "new_c")
          , RA.Purge (Id "new_b")
          , RA.Par
            [ RA.Search (Id "b") (Id "b0") [RA.NotElem (Id "c") [RA.ColumnIndex (Id "b0") 0]]
              (RA.Search (Id "d") (Id "d1") [RA.Constrain (RA.ColumnIndex (Id "d1") 0)
                                                          (RA.ColumnIndex (Id "b0") 0)]
                (RA.Project (Id "new_c") [RA.ColumnIndex (Id "b0") 0]))
            , RA.Search (Id "c") (Id "c0") [RA.NotElem (Id "b") [RA.ColumnIndex (Id "c0") 0]]
              (RA.Search (Id "d") (Id "d1") [RA.Constrain (RA.ColumnIndex (Id "d1") 0)
                                                          (RA.ColumnIndex (Id "c0") 0)]
                (RA.Project (Id "new_b") [RA.ColumnIndex (Id "c0") 0]))
            ]
          , RA.Exit [Id "new_c",Id "new_b"]
          , RA.Merge (Id "new_c") (Id "c")
          , RA.Swap (Id "new_c") (Id "delta_c")
          , RA.Merge (Id "new_b") (Id "b")
          , RA.Swap (Id "new_b") (Id "delta_b")])
        , RA.Search (Id "b") (Id "b0") []
          (RA.Search (Id "c") (Id "c1") [RA.Constrain (RA.ColumnIndex (Id "c1") 0)
                                                      (RA.ColumnIndex (Id "b0") 0)]
            (RA.Project (Id "a") [RA.ColumnIndex (Id "b0") 0]))
        ]

  -- TODO tests for rules with >2 clauses, ...
