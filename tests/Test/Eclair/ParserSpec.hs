{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.ParserSpec
  ( module Test.Eclair.ParserSpec
  ) where

import Test.Hspec
import Eclair.Parser
import NeatInterpolation


shouldParseText :: Text -> IO ()
shouldParseText txt = do
  let parseResult = parseText "test.dl" txt
  isRight parseResult `shouldBe` True

shouldParseFile :: FilePath -> IO ()
shouldParseFile file = do
  let path = "./tests/fixtures/" <> file
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

  it "parses strings in facts" $ do
    shouldParseText [text|
      @def fact(string).
      @def fact(u32, string).
      @def fact(string  ,  string,string  ).

      fact("").
      fact("a").
      fact(  "b"   ).
      fact(  "c"   , "d","e"   ).
      fact("\"\n\r\t\b\f\v\0").
      |]

  it "parses top level facts" $ do
    shouldParseFile "single_fact.dl"

  it "parses rule with 1 clause" $ do
    shouldParseFile "single_nonrecursive_rule.dl"

  it "parses rule with multiple clauses" $ do
    shouldParseFile "multiple_rule_clauses.dl"
    shouldParseFile "multiple_clauses_same_name.dl"

  it "parses assignments" $ do
    shouldParseFile "assignment.dl"

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

      literals_in_rule(x) :-
        int(  123 ),
        string( "abc"),
        mix(456, "def").
      |]

-- TODO: failure cases, more thorough testing in general
