module Eclair.AST.Lower ( compileToRA ) where

import qualified Data.Graph as G
import qualified Data.Map as M
import Eclair.AST.Codegen
import Eclair.AST.IR hiding (Clause)
import Eclair.Common.Id
import qualified Eclair.RA.IR as RA

type RA = RA.RA
type Relation = RA.Relation

compileToRA :: AST -> RA
compileToRA ast = RA.Module $ concatMap processDecls sortedDecls where
  sortedDecls = scc ast

  processDecls :: [AST] -> [RA]
  processDecls = \case
    [Atom _ name values] -> runCodegen $
      let literals = getLiterals values
       in one <$> project name (LitTerm <$> literals)
    [Rule _ name args clauses] ->
      let terms = map toTerm args
          clauses' = map toClause clauses
      in runCodegen $ processSingleRule name terms clauses'
    rules ->  -- case for multiple mutually recursive rules
      runCodegen $ processMultipleRules rules

  scc :: AST -> [[AST]]
  scc = \case
    Module _ decls -> map G.flattenSCC sortedDecls'
      where
        relevantDecls = filter isRuleOrAtom decls
        sortedDecls' = G.stronglyConnComp $ zipWith (\i d -> (d, i, refersTo d)) [0..] relevantDecls
        declLineMapping = M.fromListWith (++) $ zipWith (\i d -> (nameFor d, [i])) [0..] relevantDecls
        isRuleOrAtom = \case
          Atom {} -> True
          Rule {} -> True
          _ -> False
        refersTo :: AST -> [Int]
        refersTo = \case
          Rule _ _ _ clauses ->
            -- If no top level facts are defined, no entry exists in declLine mapping -> default to -1
            concatMap (fromMaybe [-1] . flip M.lookup declLineMapping . nameFor) $ filter isRuleOrAtom clauses
          _ -> []
        nameFor = \case
          Atom _ name _ -> name
          Rule _ name _ _ -> name
          _ ->  unreachable  -- Because of "isRuleOrAtom"
    _ -> unreachable         -- Because rejected by parser
    where unreachable = panic "Unreachable code in 'scc'"

getLiterals :: [AST] -> [Word32]
getLiterals = mapMaybe $ \case
  Lit _ lit ->
    case lit of
      LNumber x ->
        Just x
      LString _ ->
        panic "Unexpected string literal in 'getLiterals'"
  _ ->
    Nothing

-- NOTE: These rules can all be evaluated in parallel inside the fixpoint loop
processMultipleRules :: [AST] -> CodegenM [RA]
processMultipleRules rules = sequence stmts where
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
  relations = map (\(r, _, _) -> r) rulesInfo
  -- TODO: better func name
  f (r, map toTerm -> ts, map toClause -> clauses) =
    recursiveRuleToStmt r ts clauses

processSingleRule :: Relation -> [Term] -> [Clause] -> CodegenM [RA]
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
      in sequence stmts
  | otherwise = one <$> ruleToStmt relation terms clauses

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
      ConstrainClause constraint ->
        case constraint of
          NotElem r values ->
            noElemOf r values inner
      BinaryConstraint op lhs rhs -> do
        if' op lhs rhs inner

isRecursive :: Relation -> [Clause] -> Bool
isRecursive ruleName clauses =
  let atomClauses = flip mapMaybe clauses $ \case
        AtomClause name _ -> Just name
        _ -> Nothing
   in ruleName `elem` atomClauses

extractRuleData :: AST -> Maybe (Relation, [AST], [AST])
extractRuleData = \case
  Rule _ name args clauses -> Just (name, args, clauses)
  _ -> Nothing

newRelationOf, deltaRelationOf :: Relation -> Relation
deltaRelationOf = prependToId deltaPrefix
newRelationOf = prependToId newPrefix
