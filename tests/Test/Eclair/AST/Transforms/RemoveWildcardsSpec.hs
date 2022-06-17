{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.AST.Transforms.RemoveWildcardsSpec
  ( module Test.Eclair.AST.Transforms.RemoveWildcardsSpec
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
  it "replaces each wildcard with unique variable" $ do
    ast <- rewrite "wildcards"
    ast `shouldBe` [text|
      @def a(u32).

      @def b(u32, u32, u32).

      a(x) :-
        b(x, @wildcard_0, y),
        b(x, @wildcard_1, @wildcard_2),
        b(@wildcard_3, @wildcard_4, @wildcard_5).
      |]

  it "leaves normal variables untouched" $ do
    ast <- rewrite "single_recursive_rule"
    ast `shouldBe` [text|
     @def edge(u32, u32).

     @def path(u32, u32).

     edge(1, 2).

     path(x, y) :-
       edge(x, z),
       path(z, y).
      |]
