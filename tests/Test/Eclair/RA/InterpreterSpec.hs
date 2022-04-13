{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.RA.InterpreterSpec
  ( module Test.Eclair.RA.InterpreterSpec
  ) where


import Eclair.Id
import Eclair.Parser
import Eclair.AST.IR
import Eclair.AST.Lower
import Eclair.RA.IR
import Eclair.RA.Interpreter
import Eclair.TypeSystem
import Protolude hiding ((<.>))
import Test.Hspec
import qualified Data.Map as M
import System.FilePath
import NeatInterpolation

type Record = [Number]

interpret :: Text -> IO (M.Map Relation [Record])
interpret txt = do
  case parseText "<test>" txt of
    Left err -> do
      printParseError err
      panic "Failed to parse file."
    Right ast ->
      case typeCheck ast of
        Left errs -> do
          print errs
          panic "Failed to type-check file."
        Right ra -> interpretRA $ compileToRA ast

spec :: Spec
spec = describe "RA interpreter" $ parallel $ do
  it "can interpret simple facts" $ do
    result <- interpret [text|
      @def edge(u32, u32).
        @def another(u32, u32, u32).

      edge(1, 2).
        edge(2, 3).

      another(1,2,3).
      |]
    result `shouldBe` M.fromList
      [ (Id "edge", [[1,2], [2,3]])
      , (Id "another", [[1,2,3]])
      ]

  it "can interpret non recursive rules" $ do
    result <- interpret [text|
      @def a(u32, u32).
      @def b(u32, u32).
      @def c(u32).
      @def d(u32).
      @def e(u32).
      @def f(u32).

      a(1,2).

      b(x,y) :- a(x,y).

      c(3).
      c(4).

      d(x) :- c(x).

      e(4).
      e(5).

      f(x) :- c(x), e(x).
      |]

    result `shouldBe`
      M.fromList
      [ (Id "a", [[1,2]])
      , (Id "b", [[1,2]])
      , (Id "c", [[3], [4]])
      , (Id "d", [[4], [3]])
      , (Id "e", [[4], [5]])
      , (Id "f", [[4]])
      ]

  it "can interpret rules with multiple clauses" $ do
    result <- interpret [text|
      @def fact(u32).
        @def product(u32, u32).

      fact(1).
        fact(2).
          fact(3).

      product(x, y) :-
        fact(x),
        fact(y).
      |]
    result `shouldBe`
      M.fromList
      [ (Id "fact", [[1], [2], [3]])
      , (Id "product", [ [3,3], [3,2], [3,1]
                       , [2,3], [2,2], [2,1]
                       , [1,3], [1,2], [1,1]])
      ]

  it "can interpret rules with multiple clauses of same name" $ do
    result <- interpret [text|
      @def link(u32, u32).
        @def chain(u32, u32, u32).

      link(1,2).
        link(2,3).
          link(3,4).

      chain(x, y, z) :-
        link(x, y),
        link(y, z).
      |]
    result `shouldBe`
      M.fromList
      [ (Id "link", [[1,2], [2,3], [3, 4]])
      , (Id "chain", [[2,3,4], [1,2,3]])
      ]

  it "can interpret a single recursive rule" $ do
    result <- interpret [text|
      @def edge(u32, u32).
      @def path(u32, u32).

      edge(1,2).
      edge(2,3).
      edge(3,4).

      edge(5,6).

      path(x, y) :- edge(x, y).
      path(x, y) :-
        edge(x, z),
        path(z, y).
      |]
    result `shouldBe` M.fromList
      [ (Id "edge", [[1,2], [2,3], [3,4], [5,6]])
      , (Id "path", [[1,4], [2,4], [1,3], [5,6], [3,4], [2,3], [1,2]])
      ]

  it "can interpret mutually recursive rules" $ do
    result <- interpret [text|
      @def a(u32).
      @def b(u32).
      @def c(u32).
      @def d(u32).

      a(x) :- b(x), c(x).

      b(1).
      b(x) :- c(x), d(x).

      c(2).
      c(x) :- b(x), d(x).

      d(1).
      d(2).
      d(3).
      |]
    result `shouldBe` M.fromList
      [ (Id "a", [[1], [2]])
      , (Id "b", [[2], [1]])
      , (Id "c", [[1], [2]])
      , (Id "d", [[1], [2], [3]])
      ]
