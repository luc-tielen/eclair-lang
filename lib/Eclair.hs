module Eclair ( compile, run ) where

import Protolude hiding (swap)
import Protolude.Unsafe (unsafeFromJust)
import Eclair.Syntax
import Eclair.Parser
import Eclair.RA.Codegen
import Eclair.RA.IR
import Control.Lens hiding (Equality, Index)
import qualified Data.Map as M
import qualified Data.Text as T


compileRA :: AST -> RA
compileRA ast = RAModule $ concatMap processDecls sortedDecls where
  sortedDecls = scc ast

  processDecls :: [AST] -> [RA]
  processDecls = \case
    [Atom name values] -> runCodegen $
      emit $ project name (LitTerm <$> values ^.. folded . _Lit)
    [Rule name args clauses] ->
      let terms = map toTerm args
          clauses' = map toClause clauses
      in runCodegen $ processSingleRule name terms clauses'
    rules ->  -- TODO: other cases!
      []
  processSingleRule relation terms clauses
    | isRecursive relation clauses =
      -- TODO: handle case where last isn't an atom (not possible yet, but it will be later)
      let (clause@(AtomClause cName cTerms) : rest) = reverse clauses
          deltaRelation = prependToId "delta_" relation
          newRelation = prependToId "new_" relation
          stmts =
            [ merge relation deltaRelation
            , loop
              [ purge newRelation
              , flip (foldl' (processRuleClause relation)) (clause:rest) $
                  project newRelation terms
              , exit [newRelation]
              , merge newRelation relation
              , swap newRelation deltaRelation
              ]
            ]
       in traverse_ emit stmts
    | otherwise = do
      -- TODO: handle case where last isn't an atom (not possible yet, but it will be later)
      let (clause@(AtomClause cName cTerms) : rest) = reverse clauses
      emit $ do
        flip (foldl' (processRuleClause relation)) (clause:rest) $
          project relation terms
  processRuleClause relation inner = \case
    AtomClause name terms ->
      let relation' =
            if name `startsWithId` relation
              then prependToId "delta_" name
              else name
       in search relation' terms inner
  isRecursive _name _clauses =
    True -- TODO

compile :: FilePath -> IO (Either ParseError RA)
compile path = do
  map compileRA <$> parseFile path

run :: FilePath -> IO ()
run path = compile path >>= \case
  Left err -> printParseError err
  Right ast -> print ast
