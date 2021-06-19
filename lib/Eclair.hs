module Eclair ( run ) where

import Protolude
import Protolude.Unsafe (unsafeFromJust)
import Eclair.Parser
import qualified Data.Graph as G
import qualified Data.Map as M


type Relation = Id
type RAClause = RA
type Action = RA
type RAValue = (Int, Int)  -- TODO make generic -> List?

data RA
  = Insert RA
  | Search Relation [RAClause] Action
  | Project Relation [RAValue]
  | Merge Relation Relation
  | Swap Relation Relation
  | Purge Relation
  | Seq RA RA
  | Par [RA]
  | Loop RA
  | Exit RAClause
  deriving (Eq, Show)


{-
TODO: compile the following:

edge(1, 2).
edge(2, 3).

path(X, Y) :− edge(X, Y).
path(X, Z) :−
  path(X ,Y),
  edge(Y , Z).
-}

scc :: AST -> AST
scc = \case
  Module [decls] -> Module $ map G.flattenSCC sortedDecls where
    sortedDecls = G.stronglyConnComp $ zipWith (\i d -> (d, i, refersTo d)) [0..] decls
    declLineMapping = M.fromListWith (++) $ zipWith (\i d -> (nameFor d, [i])) [0..] decls
    refersTo = \case
      Rule _ _ clauses -> concatMap (unsafeFromJust . flip M.lookup declLineMapping . nameFor) clauses
      _ -> []
    -- TODO use traversals?
    nameFor = \case
      Atom name _ -> name
      Rule name _ _ -> name
      _ -> Id ""  -- TODO how to handle?
  ast -> ast

compile :: FilePath -> IO (Either ParseError RA)
compile path = do
  result <- parseFile path
  case result of
    Left err -> pure $ Left err
    Right ast -> do
      let res = scc ast
      print res -- TODO remove
      -- TODO convert to RA
      pure $ Right $ Insert $ Project (Id "edge") [(1, 2), (2, 3)]

run :: FilePath -> IO ()
run path = compile path >>= \case
  Left err -> printParseError err
  Right ast -> print ast
