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
      , UnknownAtom (NodeId 3) $ Id "path"
      , UnknownAtom (NodeId 6) $ Id "edge"
      ] [text|
      top_level_atom(1).

      path(x, y) :-
        edge(x, y).
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
      [ ArgCountMismatch (Id "b") (NodeId 2, 1) (NodeId 7, 2)
      , ArgCountMismatch (Id "c") (NodeId 3, 1) (NodeId 10, 2)
      ] [text|
      @def a(u32, u32).
      @def b(u32).
      @def c(u32).

      a(x, y) :-
        b(x, 123),
        c(y, 456).
      |]
