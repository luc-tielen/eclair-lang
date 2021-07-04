{-# LANGUAGE ViewPatterns #-}

module Eclair ( compile, run ) where

import Protolude hiding (swap)
import Protolude.Unsafe (unsafeFromJust)
import Eclair.Syntax hiding (Clause)
import Eclair.Parser
import Eclair.RA.Codegen
import Control.Lens hiding (Equality, Index)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Eclair.RA.IR as RA

type RA = RA.RA
type Relation = RA.Relation

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
    rules ->  -- case for multiple mutually recursive rules
      runCodegen $ processMultipleRules rules

-- NOTE: These rules can all be evaluated in parallel inside the fixpoint loop
processMultipleRules :: [AST] -> CodegenM ()
processMultipleRules rules = traverse_ emit stmts where
  stmts = mergeStmts ++ [loop (purgeStmts ++ ruleStmts ++ [exitStmt] ++ endLoopStmts)]
  mergeStmts = map (\r -> merge r (deltaRelationOf r)) relations
  purgeStmts = map (purge . newRelationOf) relations
  ruleStmts = [parallel $ map f rulesInfo]
  exitStmt = exit $ map newRelationOf relations
  endLoopStmts = concatMap toMergeAndSwapStmts relations
  toMergeAndSwapStmts r =
    let newRelation = newRelationOf r
        deltaRelation = deltaRelationOf r
     in [merge newRelation r, swap newRelation deltaRelation]
  rulesInfo = mapMaybe extractRuleData rules
  relations = map (view _1) rulesInfo
  -- TODO: better func name
  f (r, map toTerm -> ts, map toClause -> clauses) =
    ruleToStmt r ts clauses
    -- TODO: check if notelem is always needed? dont think so?
    --where
      --deltaRelation = deltaRelationOf r
      --newRelation = newRelationOf r
      --extraClauses = [ConstrainClause (NotElem r ts)]
      --allClauses = clauses ++ extraClauses

processSingleRule :: Relation -> [Term] -> [Clause] -> CodegenM ()
processSingleRule relation terms clauses
  | isRecursive relation clauses =
    let deltaRelation = deltaRelationOf relation
        newRelation = newRelationOf relation
        extraClauses = [ConstrainClause (NotElem relation terms)]
        allClauses = clauses ++ extraClauses
        stmts =
          [ merge relation deltaRelation
          , loop
            [ purge newRelation
            , ruleToStmt relation terms allClauses
            , exit [newRelation]
            , merge newRelation relation
            , swap newRelation deltaRelation
            ]
          ]
      in traverse_ emit stmts
  | otherwise = emit $ ruleToStmt relation terms clauses

ruleToStmt :: Relation -> [Term] -> [Clause] -> CodegenM RA
ruleToStmt relation terms clauses
  | isRecursive relation clauses =
    let newRelation = newRelationOf relation
        extraClauses = [ConstrainClause (NotElem relation terms)]
        allClauses = clauses ++ extraClauses
      in transform relation newRelation terms allClauses
  | otherwise = transform relation relation terms clauses
  where
    transform relation intoRelation terms clauses =
      flip (foldr (processRuleClause relation)) clauses $
        project intoRelation terms
    processRuleClause ruleName clause inner = case clause of
      AtomClause clauseName terms ->
        let relation' =
              if clauseName `startsWithId` ruleName
                then prependToId deltaPrefix clauseName
                else clauseName
        in search relation' terms inner
      ConstrainClause (NotElem r values) ->
        noElemOf r values inner

isRecursive :: Relation -> [Clause] -> Bool
isRecursive ruleName clauses =
  ruleName `elem` map (\(AtomClause name _) -> name) clauses

extractRuleData :: AST -> Maybe (Relation, [AST], [AST])
extractRuleData = \case
  Rule name args clauses -> Just (name, args, clauses)
  _ -> Nothing

newRelationOf, deltaRelationOf :: Relation -> Relation
deltaRelationOf = prependToId deltaPrefix
newRelationOf = prependToId newPrefix

compile :: FilePath -> IO (Either ParseError RA)
compile path = do
  map compileRA <$> parseFile path

run :: FilePath -> IO ()
run path = compile path >>= \case
  Left err -> printParseError err
  Right ast -> print ast
