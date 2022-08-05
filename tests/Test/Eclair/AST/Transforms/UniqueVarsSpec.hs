{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.AST.Transforms.UniqueVarsSpec
  ( module Test.Eclair.AST.Transforms.UniqueVarsSpec
  ) where

import Test.Hspec
import NeatInterpolation
import System.FilePath
import Eclair
import Eclair.Pretty

rewrite :: FilePath -> IO Text
rewrite path = do
  let file = "tests/fixtures" </> path <.> "dl"
  eir <- transformAST file
  pure $ printDoc eir


spec :: Spec
spec = describe "remove wildcards transform" $ parallel $ do
  it "replaces each variable that occurs more than one time in a clause with unique variables" $ do
    ast <- rewrite "clause_with_same_vars"
    ast `shouldBe` [text|
      @def a(u32).

      @def b(u32, u32).

      @def c(u32, u32, u32, u32, u32).

      @def other(u32).

      a(x) :-
        b(x, @x_0),
        other(x),
        x = @x_0.

      a(y) :-
        c(y, @y_0, 42, x, @y_1),
        other(y),
        y = @y_1,
        y = @y_0.
      |]

  it "leaves variables that appear only one time untouched" $ do
    ast <- rewrite "single_recursive_rule"
    ast `shouldBe` [text|
     @def edge(u32, u32).

     @def path(u32, u32).

     edge(1, 2).

     path(x, y) :-
       edge(x, z),
       path(z, y).
      |]
