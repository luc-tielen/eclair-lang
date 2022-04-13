{-# LANGUAGE QuasiQuotes #-}

module Test.Eclair.TypeSystemSpec
  ( module Test.Eclair.TypeSystemSpec
  ) where

import Protolude hiding (TypeError)
import Test.Hspec
import Eclair.TypeSystem
import Eclair.Parser
import Eclair.Id
import NeatInterpolation


tc :: Text -> Either [TypeError] TypeInfo
tc code = do
  let parseResult = parseText "<test>" code
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
    failsWith [DuplicateTypeDeclaration (Id "edge")] [text|
      @def edge(u32, u32).
      @def edge(u32, u32).
      |]
    failsWith [DuplicateTypeDeclaration (Id "edge")] [text|
      @def edge(u32, u32).
      @def edge(u32).
      @def edge(u32, u32, u32).
      |]
    failsWith
      [ DuplicateTypeDeclaration (Id "edge")
      , DuplicateTypeDeclaration (Id "path")] [text|
      @def edge(u32, u32).
      @def path(u32, u32).
      @def edge(u32, u32).
      @def path(u32, u32).
      |]

  it "checks for unknown top level atoms" $ do
    failsWith [UnknownAtom $ Id "edge"] [text|
      edge(1, 2).
      |]
    failsWith [UnknownAtom $ Id "edge", UnknownAtom $ Id "path"] [text|
      edge(1, 2).
      path(3, 4).
      |]

  it "checks for unknown atoms in rule heads" $ do
    failsWith [UnknownAtom $ Id "path"] [text|
      @def edge(u32, u32).

      path(x, y) :-
        edge(x, y).
      |]

  it "checks for unknown atoms in rule bodies" $ do
    failsWith [UnknownAtom $ Id "edge"] [text|
      @def path(u32, u32).

      path(x, y) :-
        edge(x, y).
      |]

  it "checks for unknown atoms in mix of everything" $ do
    failsWith
      [ UnknownAtom $ Id "top_level_atom"
      , UnknownAtom $ Id "path"
      , UnknownAtom $ Id "edge"
      ] [text|
      top_level_atom(1).

      path(x, y) :-
        edge(x, y).
      |]

  it "checks for mismatching argument count in top level atoms" $ do
    failsWith [ArgCountMismatch (Id "edge") 3 2] [text|
      @def edge(u32, u32, u32).
      edge(1, 2).
      |]
    failsWith
      [ ArgCountMismatch (Id "edge") 3 2
      , ArgCountMismatch (Id "path") 1 2
      ] [text|
      @def edge(u32, u32, u32).
      @def path(u32).
      edge(1, 2).
      path(3, 4).
      |]

  it "checks for mismatching argument count in rule heads" $ do
    failsWith [ArgCountMismatch (Id "path") 3 2] [text|
      @def edge(u32, u32).
      @def path(u32, u32, u32).

      path(x, y) :-
        edge(x, y).
      |]
    failsWith [ArgCountMismatch (Id "path") 1 2] [text|
      @def edge(u32, u32).
      @def path(u32).

      path(x, y) :-
        edge(x, y).
      |]

  it "checks for mismatching argument count in rule bodies" $ do
    failsWith [ArgCountMismatch (Id "edge") 3 2] [text|
      @def edge(u32, u32, u32).
      @def path(u32, u32).

      path(x, y) :-
        edge(x, y).
      |]
    failsWith [ArgCountMismatch (Id "edge") 2 3] [text|
      @def edge(u32, u32).
      @def path(u32, u32).

      path(x, y) :-
        edge(x, y, 123).
      |]
    failsWith
      [ ArgCountMismatch (Id "b") 1 2
      , ArgCountMismatch (Id "c") 1 2
      ] [text|
      @def a(u32, u32).
      @def b(u32).
      @def c(u32).

      a(x, y) :-
        b(x, 123),
        c(y, 456).
      |]
