module Eclair.Lowering.AST ( compileRA ) where

import Protolude hiding (swap)
import Eclair.Syntax hiding (Clause)
import Eclair.RA.Codegen
import qualified Eclair.RA.IR as RA
import Control.Lens hiding (Equality, Index)

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
    recursiveRuleToStmt r ts clauses

processSingleRule :: Relation -> [Term] -> [Clause] -> CodegenM ()
processSingleRule relation terms clauses
  | isRecursive relation clauses =
    let deltaRelation = deltaRelationOf relation
        newRelation = newRelationOf relation
        stmts =
          [ merge relation deltaRelation
          , loop
            [ purge newRelation
            , ruleToStmt relation terms clauses
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
    recursiveRuleToStmt relation terms clauses
  | otherwise = nestedSearchAndProject relation relation terms clauses

recursiveRuleToStmt :: Relation -> [Term] -> [Clause] -> CodegenM RA
recursiveRuleToStmt relation terms clauses =
  let newRelation = newRelationOf relation
      extraClauses = [ConstrainClause (NotElem relation terms)]
      allClauses = clauses ++ extraClauses
    in nestedSearchAndProject relation newRelation terms allClauses

nestedSearchAndProject :: Relation -> Relation -> [Term] -> [Clause] -> CodegenM RA
nestedSearchAndProject relation intoRelation terms clauses =
  flip (foldr (processRuleClause relation)) clauses $
    project intoRelation terms
  where
    processRuleClause ruleName clause inner = case clause of
      AtomClause clauseName args ->
        let relation' =
              if clauseName `startsWithId` ruleName
                then prependToId deltaPrefix clauseName
                else clauseName
        in search relation' args inner
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
