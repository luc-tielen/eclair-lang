module Eclair ( compile, run ) where

import Protolude hiding (swap)
import Protolude.Unsafe (unsafeFromJust)
import Eclair.Syntax
import Eclair.Parser
import Eclair.RA.Codegen
import Control.Lens hiding (Equality, Index)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Eclair.RA.IR as RA

type RA = RA.RA

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
      let deltaRelation = prependToId deltaPrefix relation
          newRelation = prependToId newPrefix relation
          extraClauses = [ConstrainClause (NotElem relation terms)]
          allClauses = clauses ++ extraClauses
          stmts =
            [ merge relation deltaRelation
            , loop
              [ purge newRelation
              , flip (foldr (processRuleClause relation)) allClauses $
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
              then prependToId deltaPrefix clauseName
              else clauseName
       in search relation' terms inner
    ConstrainClause (NotElem r values) ->
      noElemOf r values inner
  isRecursive ruleName clauses =
    ruleName `elem` map (\(AtomClause name _) -> name) clauses

compile :: FilePath -> IO (Either ParseError RA)
compile path = do
  map compileRA <$> parseFile path

run :: FilePath -> IO ()
run path = compile path >>= \case
  Left err -> printParseError err
  Right ast -> print ast
