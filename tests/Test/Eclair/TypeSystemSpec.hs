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
  -- NOTE: though this should give a dead code error somewhere else
  it "typechecks if there are no rules found for a type declaration" $ do
    typeChecks [text|
      @def edge(u32, u32).
      @def chain(u32, u32, u32).
      |]

  it "returns type errors for duplicate type declarations" $ do
    typeChecks [text|
      @def edge(u32, u32).
      @def path(u32, u32).
      |]
    failsWith [DuplicateTypeDeclaration (Id "edge") (fromList [(NodeId 1, [U32, U32]), (NodeId 2, [U32, U32])])] [text|
      @def edge(u32, u32).
      @def edge(u32, u32).
      |]
    failsWith [DuplicateTypeDeclaration (Id "edge") (fromList [(NodeId 1, [U32, U32]), (NodeId 2, [U32]), (NodeId 3, [U32, U32, U32])])] [text|
      @def edge(u32, u32).
      @def edge(u32).
      @def edge(u32, u32, u32).
      |]
    failsWith
      [ DuplicateTypeDeclaration (Id "edge") (fromList [(NodeId 1, [U32, U32]), (NodeId 3, [U32, U32])])
      , DuplicateTypeDeclaration (Id "path") (fromList [(NodeId 2, [U32, U32]), (NodeId 4, [U32, U32])])] [text|
      @def edge(u32, u32).
      @def path(u32, u32).
      @def edge(u32, u32).
      @def path(u32, u32).
      |]

  it "checks for unknown top level atoms" $ do
    failsWith [UnknownAtom (NodeId 1) $ Id "edge"] [text|
      edge(1, 2).
      |]
    failsWith [UnknownAtom (NodeId 1) $ Id "edge", UnknownAtom (NodeId 4) $ Id "path"] [text|
      edge(1, 2).
      path(3, 4).
      |]

  it "checks for unknown atoms in rule heads" $ do
    failsWith [UnknownAtom (NodeId 2) $ Id "path"] [text|
      @def edge(u32, u32).

      path(x, y) :-
        edge(x, y).
      |]

  it "checks for unknown atoms in rule bodies" $ do
    failsWith [UnknownAtom (NodeId 5) $ Id "edge"] [text|
      @def path(u32, u32).

      path(x, y) :-
        edge(x, y).
      |]

  it "checks for unknown atoms in mix of everything" $ do
    failsWith
      [ UnknownAtom (NodeId 1) $ Id "top_level_atom"
      , UnknownAtom (NodeId 6) $ Id "edge"
      , UnknownAtom (NodeId 3) $ Id "path"
      ] [text|
      top_level_atom(1).

      path(x, y) :-
        edge(x, y).
      |]

  it "checks for type mismatch in top level atoms" $ do
    failsWith
      [ TypeMismatch (NodeId {unNodeId = 5}) U32 Str
      , TypeMismatch (NodeId {unNodeId = 7}) Str U32
      , TypeMismatch (NodeId {unNodeId = 11}) U32 Str
      , TypeMismatch (NodeId {unNodeId = 10}) Str U32
      , TypeMismatch (NodeId {unNodeId = 14}) U32 Str
      , TypeMismatch (NodeId {unNodeId = 13}) U32 Str
      ] [text|
      @def fact1(u32, string).
      @def fact2(string, string).
      fact1(1, 2).
      fact1("abc", "def").
      fact1("abc", 2).
      fact2(1, 2).
      |]
    typeChecks [text|
      @def fact1(u32, string).
      @def fact2(string, string).
      fact1(1, "a").
      fact2("abc", "def").
      |]

  it "checks for type mismatch in rule heads" $ do
    failsWith
      [ TypeMismatch (NodeId {unNodeId = 5}) Str U32
      , TypeMismatch (NodeId {unNodeId = 10}) Str U32
      , TypeMismatch (NodeId {unNodeId = 17}) Str U32
      , TypeMismatch (NodeId {unNodeId = 16}) Str U32
      ] [text|
      @def edge(u32, u32).
      @def reachable(u32, u32).
      reachable(x, "abc") :- edge(x, x).
      reachable("abc", x) :- edge(x, x).
      reachable("abc", "abc") :- edge(x, x).
      |]
    typeChecks [text|
      @def edge(u32, u32).
      @def reachable(u32, u32).
      reachable(x, y) :- edge(x, y).
      reachable(1, 2) :- edge(x, x).
      |]

  it "checks for type mismatch in rule bodies" $ do
    failsWith
      [ TypeMismatch (NodeId {unNodeId = 13}) Str U32
      , TypeMismatch (NodeId {unNodeId = 12}) Str U32
      , TypeMismatch (NodeId {unNodeId = 10}) Str U32
      , TypeMismatch (NodeId {unNodeId = 6}) Str U32
      ] [text|
      @def fact1(u32, u32).
      @def fact2(u32).
      fact2(123) :-
        fact1("abc", 123),
        fact1(456, "def"),
        fact1("abc", "def").
      |]
    typeChecks [text|
      @def fact1(u32, u32).
      @def fact2(u32).
      fact2(123) :-
        fact1(123, 456).
      |]

  it "checks for type mismatch of variables in rule heads" $ do
    failsWith
      [ TypeMismatch (NodeId {unNodeId = 5}) U32 Str
      ] [text|
      @def fact1(u32).
      @def fact2(u32, string).
      fact2(x, x) :-
        fact1(123).
      |]

  it "checks for type mismatch of variables in rule bodies" $ do
    failsWith
      [ TypeMismatch (NodeId {unNodeId = 7}) U32 Str
      ] [text|
      @def fact1(u32).
      @def fact2(u32, string).
      fact1(123) :-
        fact2(x, x).
      |]
    typeChecks [text|
      @def fact1(u32).
      @def fact2(u32, u32).
      fact1(123) :-
        fact2(x, x).
      |]

  it "checks for type mismatch of variables in entire rule" $ do
    failsWith
      [ TypeMismatch (NodeId {unNodeId = 7}) Str U32
      ] [text|
      @def fact1(u32).
      @def fact2(u32, string).
      fact2(123, x) :-
        fact1(x).
      |]
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
      [UnificationFailure U32 Str
      ] [text|
        @def fact1(u32, u32).
        @def fact2(u32).
        fact2(x) :-
          fact1(x, _),
          x = "abc".
      |]
    failsWith
      [UnificationFailure Str U32
      ] [text|
        @def fact1(u32, u32).
        @def fact2(u32).
        fact2(x) :-
          fact1(x, _),
          "abc" = x.
      |]
    failsWith
      [UnificationFailure Str U32
      ] [text|
        @def fact1(string, string).
        @def fact2(string).
        fact2(x) :-
          fact1(x, _),
          x = 1.
      |]
    failsWith
      [UnificationFailure U32 Str
      ] [text|
        @def fact1(string, string).
        @def fact2(string).
        fact2(x) :-
          fact1(x, _),
          1 = x.
      |]

  it "emits a type error when a variable fails to unify with another variable" $ do
    failsWith
      [ UnificationFailure Str U32
      ] [text|
        @def fact1(string, u32).
        @def fact2(string).
        fact2(x) :-
          fact1(x, y),
          x = y.
      |]
    failsWith
      [ UnificationFailure U32 Str
      ] [text|
      @def fact1(u32, string).
      @def fact2(u32).
      fact2(x) :-
        x = y,
        fact1(x, z),
        y = z.
      |]
    failsWith
      [ UnificationFailure Str U32
      ] [text|
      @def fact1(u32, string).
      @def fact2(u32).
      fact2(x) :-
        x = y,
        y = z,
        fact1(x, z).
      |]
