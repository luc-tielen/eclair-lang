module Eclair ( compile, run ) where

import Protolude hiding (swap)
import Protolude.Unsafe (unsafeFromJust)
import Eclair.Syntax
import Eclair.Parser
import Eclair.RA.Codegen
import Eclair.RA.IR as RA
import Control.Lens hiding (Equality, Index)
import qualified Data.Map as M
import qualified Data.Text as T


compileRA :: AST -> RA
compileRA ast = RA.Module $ concatMap processDecls sortedDecls where
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
      let deltaRelation = prependToId "delta_" relation
          newRelation = prependToId "new_" relation
          stmts =
            [ merge relation deltaRelation
            , loop
              [ purge newRelation
              , flip (foldr (processRuleClause relation)) clauses $
                  project newRelation terms
              , exit [newRelation]
              , merge newRelation relation
              , swap newRelation deltaRelation
              ]
            ]
       in traverse_ emit stmts
    | otherwise = do
      emit $ do
        flip (foldr (processRuleClause relation)) clauses $
          project relation terms
  processRuleClause ruleName clause inner = case clause of
    AtomClause clauseName terms ->
      let relation' =
            if clauseName `startsWithId` ruleName
              then prependToId "delta_" clauseName
              else clauseName
       in search relation' terms inner
  isRecursive ruleName clauses =
    ruleName `elem` map (\(AtomClause name _) -> name) clauses

compile :: FilePath -> IO (Either ParseError RA)
compile path = do
  map compileRA <$> parseFile path

run :: FilePath -> IO ()
run path = compile path >>= \case
  Left err -> printParseError err
  Right ast -> print ast
