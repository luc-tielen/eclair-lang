{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.ParserSpec
  ( module Test.Eclair.ParserSpec
  ) where

import Protolude
import Test.Hspec
import Eclair.Parser
import NeatInterpolation


shouldParseText :: Text -> IO ()
shouldParseText txt = do
  let parseResult = parseText "test.dl" txt
  isRight parseResult `shouldBe` True

shouldParseFile :: FilePath -> IO ()
shouldParseFile file = do
  let path = "./tests/fixtures/codegen/" <> file
  result <- parseFile path
  isRight result `shouldBe` True


spec :: Spec
spec = describe "parsing" $ parallel $ do
  it "parses type declarations" $ do
    shouldParseText [text|
      @def fact(u32).
      @def fact(u32, u32, u32).
       @def  fact  (  u32  ,  u32  ).
      |]

  it "parses top level facts" $ do
    shouldParseFile "single_fact.dl"

  it "parses rule with 1 clause" $ do
    shouldParseFile "single_nonrecursive_rule.dl"

  it "parses rule with multiple clauses" $ do
    shouldParseFile "multiple_rule_clauses.dl"
    shouldParseFile "multiple_clauses_same_name.dl"

  it "parses mix of everything" $ do
    shouldParseText [text|
      @def edge(u32, u32).
      @def path(u32, u32).

      edge(1, 2).
      edge(2, 3).

      path(x, y) :-
        edge(x, y).

      path(x, y) :-
        edge(x, z),
        path(z, y).
      |]

-- TODO: failure cases, more thorough testing in general
