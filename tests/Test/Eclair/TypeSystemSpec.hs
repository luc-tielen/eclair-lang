{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.TypeSystemSpec
  ( module Test.Eclair.TypeSystemSpec
  ) where

import Test.Hspec
import Eclair.TypeSystem
import Eclair.Parser
import Eclair.Id
import Eclair.AST.IR (NodeId(..))
import NeatInterpolation


tc :: Text -> Either [TypeError] TypeInfo
tc code = do
  let parseResult = map (\(ast, _, _) -> ast) $ parseText "<test>" code
  case parseResult of
    Left _ -> panic "Failed to parse code!"
    Right ast -> typeCheck ast

typeChecks :: Text -> IO ()
typeChecks code =
  isRight (tc code) `shouldBe` True

failsWith :: [TypeError] -> Text -> IO ()
failsWith errs code =
  tc code `shouldBe` Left errs

spec :: Spec
spec = describe "typesystem" $ parallel $ do
  it "checks for type mismatch in top level atoms" $ do
    typeChecks [text|
      @def fact1(u32, string).
      @def fact2(string, string).
      fact1(1, "a").
      fact2("abc", "def").
      |]

  it "checks for type mismatch in rule heads" $ do
    typeChecks [text|
      @def edge(u32, u32).
      @def reachable(u32, u32).
      reachable(x, y) :- edge(x, y).
      reachable(1, 2) :- edge(x, x).
      |]

  it "checks for type mismatch in rule bodies" $ do
    typeChecks [text|
      @def fact1(u32, u32).
      @def fact2(u32).
      fact2(123) :-
        fact1(123, 456).
      |]

  it "checks for type mismatch of variables in rule bodies" $ do
    typeChecks [text|
      @def fact1(u32).
      @def fact2(u32, u32).
      fact1(123) :-
        fact2(x, x).
      |]

  it "checks for type mismatch of variables in entire rule" $ do
    typeChecks [text|
      @def fact1(u32).
      @def fact2(u32, string).
      @def fact3(string).
      fact2(x, y) :-
        fact1(x),
        fact3(y).
      |]

  it "checks for mismatching argument count in top level atoms" $ do
    failsWith [ArgCountMismatch (Id "edge") (NodeId 1, 3) (NodeId 2, 2)] [text|
      @def edge(u32, u32, u32).
      edge(1, 2).
      |]
    failsWith
      [ ArgCountMismatch (Id "edge") (NodeId 1, 3) (NodeId 3, 2)
      , ArgCountMismatch (Id "path") (NodeId 2, 1) (NodeId 6, 2)
      ] [text|
      @def edge(u32, u32, u32).
      @def path(u32).
      edge(1, 2).
      path(3, 4).
      |]

  it "checks for mismatching argument count in rule heads" $ do
    failsWith [ArgCountMismatch (Id "path") (NodeId 2, 3) (NodeId 3, 2)] [text|
      @def edge(u32, u32).
      @def path(u32, u32, u32).

      path(x, y) :-
        edge(x, y).
      |]
    failsWith [ArgCountMismatch (Id "path") (NodeId 2, 1) (NodeId 3, 2)] [text|
      @def edge(u32, u32).
      @def path(u32).

      path(x, y) :-
        edge(x, y).
      |]

  it "checks for mismatching argument count in rule bodies" $ do
    failsWith [ArgCountMismatch (Id "edge") (NodeId 1, 3) (NodeId 6, 2)] [text|
      @def edge(u32, u32, u32).
      @def path(u32, u32).

      path(x, y) :-
        edge(x, y).
      |]
    failsWith [ArgCountMismatch (Id "edge") (NodeId 1, 2) (NodeId 6, 3)] [text|
      @def edge(u32, u32).
      @def path(u32, u32).

      path(x, y) :-
        edge(x, y, 123).
      |]
    failsWith
      [ ArgCountMismatch (Id "c") (NodeId 3, 1) (NodeId 10, 2)
      , ArgCountMismatch (Id "b") (NodeId 2, 1) (NodeId 7, 2)
      ] [text|
      @def a(u32, u32).
      @def b(u32).
      @def c(u32).

      a(x, y) :-
        b(x, 123),
        c(y, 456).
      |]

  it "never emits error on wildcards" $ do
    typeChecks [text|
      @def fact1(u32, u32, string).
      @def fact2(u32).
      fact2(x) :-
        fact1(x, _, _),
        fact1(_, x, _).
      |]

  it "checks types in equalities between a variable and a literal" $ do
    typeChecks [text|
      @def fact1(u32, u32).
      @def fact2(u32).
      fact2(x) :-
        fact1(x, _),
        x = 123.
      |]
    typeChecks [text|
      @def fact1(u32, u32).
      @def fact2(u32).
      fact2(x) :-
        fact1(x, _),
        123 = x.
      |]
    typeChecks [text|
      @def fact1(string, string).
      @def fact2(string).
      fact2(x) :-
        fact1(x, _),
        "abc" = x.
      |]
    typeChecks [text|
      @def fact1(string, string).
      @def fact2(string).
      fact2(x) :-
        fact1(x, _),
        x = "abc".
      |]

  it "checks types in equalities between two variables" $ do
    typeChecks [text|
      @def fact1(u32, u32).
      @def fact2(u32).
      fact2(x) :-
        fact1(x, y),
        x = y.
      |]
    typeChecks [text|
      @def fact1(u32, u32).
      @def fact2(u32).
      fact2(x) :-
        x = y,
        fact1(x, z),
        y = z.
      |]
    typeChecks [text|
      @def fact1(u32, u32).
      @def fact2(u32).
      fact2(x) :-
        x = y,
        y = z,
        fact1(x, z).
      |]

  it "checks types in equalities between two literals" $ do
    typeChecks [text|
      @def fact1(u32, u32).
      @def fact2(u32).
      fact2(x) :-
        fact1(x, _),
        123 = 456.
      |]
    typeChecks [text|
      @def fact1(u32, u32).
      @def fact2(u32).
      fact2(x) :-
        fact1(x, _),
        "abc" = "def".
      |]

  it "emits a type error when a variable fails to unify with type of a literal" $ do
    failsWith
      [ UnificationFailure U32 Str (WhileChecking (NodeId {unNodeId = 3}) :| [WhileChecking (NodeId {unNodeId = 8}),WhileUnifying (NodeId {unNodeId = 8})])
      ] [text|
        @def fact1(u32, u32).
        @def fact2(u32).
        fact2(x) :-
          fact1(x, _),
          x = "abc".
      |]
    failsWith
      [ UnificationFailure Str U32 (WhileChecking (NodeId {unNodeId = 3}) :| [WhileChecking (NodeId {unNodeId = 9}),WhileUnifying (NodeId {unNodeId = 9})])
      ] [text|
        @def fact1(u32, u32).
        @def fact2(u32).
        fact2(x) :-
          fact1(x, _),
          "abc" = x.
      |]
    failsWith
      [ UnificationFailure Str U32 (WhileChecking (NodeId {unNodeId = 3}) :| [WhileChecking (NodeId {unNodeId = 8}),WhileUnifying (NodeId {unNodeId = 8})])
      ] [text|
        @def fact1(string, string).
        @def fact2(string).
        fact2(x) :-
          fact1(x, _),
          x = 1.
      |]
    failsWith
      [ UnificationFailure U32 Str (WhileChecking (NodeId {unNodeId = 3}) :| [WhileChecking (NodeId {unNodeId = 9}),WhileUnifying (NodeId {unNodeId = 9})])
      ] [text|
        @def fact1(string, string).
        @def fact2(string).
        fact2(x) :-
          fact1(x, _),
          1 = x.
      |]

  it "emits a type error when a variable fails to unify with another variable" $ do
    failsWith
      [ UnificationFailure Str U32 (WhileChecking (NodeId {unNodeId = 3}) :| [WhileChecking (NodeId {unNodeId = 8}),WhileUnifying (NodeId {unNodeId = 8})])
      ] [text|
        @def fact1(string, u32).
        @def fact2(string).
        fact2(x) :-
          fact1(x, y),
          x = y.
      |]
    failsWith
      [ UnificationFailure U32 Str (WhileChecking (NodeId {unNodeId = 3}) :| [WhileChecking (NodeId {unNodeId = 11}),WhileUnifying (NodeId {unNodeId = 11})])
      ] [text|
      @def fact1(u32, string).
      @def fact2(u32).
      fact2(x) :-
        x = y,
        fact1(x, z),
        y = z.
      |]
    failsWith
      [ TypeMismatch (NodeId {unNodeId = 13}) U32 Str (WhileChecking (NodeId {unNodeId = 3}) :| [WhileChecking (NodeId {unNodeId = 13})])
      ] [text|
      @def fact1(u32, string).
      @def fact2(u32).
      fact2(x) :-
        x = y,
        y = z,
        fact1(x, z).
      |]
    failsWith
      [ TypeMismatch (NodeId {unNodeId = 16}) U32 Str (WhileChecking (NodeId {unNodeId = 3}) :| [WhileChecking (NodeId {unNodeId = 16})])
      ] [text|
      @def fact1(u32, string).
      @def fact2(u32).
      fact2(x) :-
        x = y,
        y = z,
        z = a,
        fact1(x, a).
      |]
    failsWith
      [ UnificationFailure U32 Str (WhileChecking (NodeId {unNodeId = 3}) :| [WhileChecking (NodeId {unNodeId = 14}),WhileUnifying (NodeId {unNodeId = 14})])
      ] [text|
      @def fact1(u32, string).
      @def fact2(u32).
      fact2(x) :-
        x = y,
        y = z,
        fact1(x, a),
        z = a.
      |]
