{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.AST.Transforms.ShiftAssignmentsSpec
  ( module Test.Eclair.AST.Transforms.ShiftAssignmentsSpec
  ) where

import Test.Hspec
import NeatInterpolation
import System.FilePath
import Eclair
import Eclair.Pretty

rewrite :: FilePath -> IO Text
rewrite path = do
  let file = "tests/fixtures" </> path <.> "dl"
  (ast, _) <- transformAST file
  pure $ printDoc ast


spec :: Spec
spec = describe "shift assignments transform" $ parallel $ do
  it "shifts assignments to the end of a rule body" $ do
    ast <- rewrite "assignment"
    ast `shouldBe` [text|
      @def fact1(u32, u32).

      @def fact2(u32, u32).

      fact2(x, 1) :-
        fact1(x, z),
        fact1(y, x),
        z = x,
        y = 123.

      fact2(x, y) :-
        fact1(y, x),
        123 = x.
      |]

  it "leaves normal rule clauses untouched" $ do
    ast <- rewrite "single_recursive_rule"
    ast `shouldBe` [text|
     @def edge(u32, u32).

     @def path(u32, u32).

     edge(1, 2).

     path(x, y) :-
       edge(x, z),
       path(z, y).
      |]
