module Eclair.AST.Lower
  ( compileToRA
  ) where

import Prelude hiding (swap, project)
import qualified Data.Graph as G
import qualified Data.Map as M
import Eclair.AST.Codegen
import Eclair.AST.IR hiding (Clause)
import Eclair.Common.Id
import Eclair.Common.Location (NodeId(..))
import qualified Eclair.RA.IR as RA
import Eclair.Common.Extern


type RA = RA.RA
type Relation = RA.Relation

compileToRA :: [Extern] -> AST -> RA
compileToRA externs ast =
  RA.Module (NodeId 0) $ concatMap processDecls sortedDecls
  where
    sortedDecls = scc ast

    processDecls :: [AST] -> [RA]
    processDecls = \case
      [Atom _ name values] -> runCodegen externs $
        let literals = map toTerm values
        in one <$> project name literals
      [Rule _ name args clauses] ->
        let terms = map toTerm args
        in runCodegen externs $ processSingleRule name terms clauses
      rules ->  -- case for multiple mutually recursive rules
        runCodegen externs $ processMultipleRules rules

    scc :: AST -> [[AST]]
    scc = \case
      Module _ decls -> map G.flattenSCC sortedDecls'
        where
          relevantDecls = filter isRuleOrAtom decls
          sortedDecls' = G.stronglyConnComp $ zipWith (\i d -> (d, i, refersTo d)) [0..] relevantDecls
          declLineMapping = M.fromListWith (<>) $ zipWith (\i d -> (nameFor d, [i])) [0..] relevantDecls
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

-- NOTE: These rules can all be evaluated in parallel inside the fixpoint loop
processMultipleRules :: [AST] -> CodegenM [RA]
processMultipleRules rules = sequence stmts where
  stmts = mergeStmts <> [loop (purgeStmts <> ruleStmts <> [exitStmt] <> endLoopStmts)]
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
  f (r, map toTerm -> ts, clauses) =
    recursiveRuleToStmt r ts clauses

processSingleRule :: Relation -> [CodegenM RA] -> [AST] -> CodegenM [RA]
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

ruleToStmt :: Relation -> [CodegenM RA] -> [AST] -> CodegenM RA
ruleToStmt relation terms clauses
  | isRecursive relation clauses =
    recursiveRuleToStmt relation terms clauses
  | otherwise = nestedSearchAndProject relation relation terms clauses id

recursiveRuleToStmt :: Relation -> [CodegenM RA] -> [AST] -> CodegenM RA
recursiveRuleToStmt relation terms clauses =
  let newRelation = newRelationOf relation
      extraClause = noElemOf relation terms
    in nestedSearchAndProject relation newRelation terms clauses extraClause

nestedSearchAndProject
  :: Relation
  -> Relation
  -> [CodegenM RA]
  -> [AST]
  -> (CodegenM RA -> CodegenM RA)
  -> CodegenM RA
nestedSearchAndProject relation intoRelation terms clauses wrapWithExtraClause =
  flip (foldr (processRuleClause relation)) clauses $ wrapWithExtraClause $
    project intoRelation terms
  where
    processRuleClause ruleName clause inner = case clause of
      Atom _ clauseName args -> do
        externs <- asks envExterns
        let relation' =
              if clauseName `startsWithId` ruleName
                then prependToId deltaPrefix clauseName
                else clauseName
            isExtern = isJust $ find (\(Extern name _ _) -> relation' == name) externs
        if isExtern
          then do
            clause' <- toTerm clause
            zero <- toTerm (Lit (NodeId 0) $ LNumber 0)
            if' NotEquals clause' zero inner
          else search relation' args inner
      Constraint _ op lhs rhs -> do
        lhsTerm <- toTerm lhs
        rhsTerm <- toTerm rhs
        if' op lhsTerm rhsTerm inner
      _ ->
        panic "Unexpected rule clause in 'nestedSearchAndProject'!"

isRecursive :: Relation -> [AST] -> Bool
isRecursive ruleName clauses =
  let atomNames = flip mapMaybe clauses $ \case
        Atom _ name _ -> Just name
        _ -> Nothing
   in ruleName `elem` atomNames

extractRuleData :: AST -> Maybe (Relation, [AST], [AST])
extractRuleData = \case
  Rule _ name args clauses -> Just (name, args, clauses)
  _ -> Nothing

newRelationOf, deltaRelationOf :: Relation -> Relation
deltaRelationOf = prependToId deltaPrefix
newRelationOf = prependToId newPrefix
