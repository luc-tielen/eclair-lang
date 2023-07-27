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
        in runCodegen externs $ processSingleRule [name] name terms clauses
      rules ->  -- case for multiple mutually recursive rules
        let sccNames = rules & mapMaybe (\case
              Rule _ name _ _ -> Just name
              _ -> Nothing)
        in runCodegen externs $ processMultipleRules sccNames rules

    scc :: AST -> [[AST]]
    scc = \case
      Module _ decls -> map G.flattenSCC sortedDecls'
        where
          relevantDecls = filter isRelevant decls
          sortedDecls' = G.stronglyConnComp $ zipWith (\i d -> (d, i, refersTo d)) [0..] relevantDecls
          declLineMapping = M.fromListWith (<>) $ zipWith (\i d -> (nameFor d, [i])) [0..] relevantDecls
          isRelevant = \case
            Atom {} -> True
            Rule {} -> True
            Not {} -> True
            _ -> False
          nameFor = \case
            Atom _ name _ -> name
            Rule _ name _ _ -> name
            _ ->  unreachable  -- Because of "isRelevant"
          refersTo :: AST -> [Int]
          refersTo = \case
            Rule _ _ _ clauses ->
              -- If no top level facts are defined, no entry exists in declLine mapping -> default to -1
              concatMap (fromMaybe [-1] . flip M.lookup declLineMapping . dependsOn) $ filter isRelevant clauses
            _ -> []
          dependsOn = \case
            Atom _ name _ -> name
            Rule _ name _ _ -> name
            Not _ (Atom _ name _) -> name
            _ ->  unreachable  -- Because of "isRelevant"
      _ -> unreachable         -- Because rejected by parser
      where unreachable = panic "Unreachable code in 'scc'"

-- NOTE: These rules can all be evaluated in parallel inside the fixpoint loop
processMultipleRules :: [Relation] -> [AST] -> CodegenM [RA]
processMultipleRules sccNames rules = sequence stmts where
  stmts = mergeStmts <> [loop (purgeStmts <> ruleStmts <> [exitStmt] <> endLoopStmts)]
  mergeStmts = map (\r -> merge r (deltaRelationOf r)) uniqRelations
  purgeStmts = map (purge . newRelationOf) uniqRelations
  ruleStmts = [parallel $ map lowerRule rulesInfo]
  exitStmt = exit $ map newRelationOf uniqRelations
  endLoopStmts = concatMap toMergeAndSwapStmts uniqRelations
  toMergeAndSwapStmts r =
    let newRelation = newRelationOf r
        deltaRelation = deltaRelationOf r
     in [merge newRelation r, swap newRelation deltaRelation]
  rulesInfo = mapMaybe extractRuleData rules
  relations = map (\(r, _, _) -> r) rulesInfo
  uniqRelations = uniqOrderPreserving relations
  -- TODO: better func name
  lowerRule (r, map toTerm -> ts, clauses) =
    recursiveRuleToStmts sccNames r ts clauses

processSingleRule :: [Relation] -> Relation -> [CodegenM RA] -> [AST] -> CodegenM [RA]
processSingleRule sccNames relation terms clauses
  | isRecursive sccNames clauses =
    let deltaRelation = deltaRelationOf relation
        newRelation = newRelationOf relation
        stmts =
          [ merge relation deltaRelation
          , loop
            [ purge newRelation
            , ruleToStmt sccNames relation terms clauses
            , exit [newRelation]
            , merge newRelation relation
            , swap newRelation deltaRelation
            ]
          ]
      in sequence stmts
  | otherwise = one <$> ruleToStmt sccNames relation terms clauses

ruleToStmt :: [Relation] -> Relation -> [CodegenM RA] -> [AST] -> CodegenM RA
ruleToStmt sccNames relation terms clauses
  | isRecursive sccNames clauses =
    recursiveRuleToStmts sccNames relation terms clauses
  | otherwise = nestedSearchAndProject relation terms clauses mempty

recursiveRuleToStmts :: [Relation] -> Relation -> [CodegenM RA] -> [AST] -> CodegenM RA
recursiveRuleToStmts sccNames relation terms clauses
  | sccClauseCount > 1 = parallel $
    [ stmt
    | i <- [0..sccClauseCount - 1]
    , let clauses' = transformAt i toDeltaClause clauses
    , clauses' /= clauses
    , let sccAtoms' = drop (i + 1) sccAtoms
          stmt = nestedSearchAndProject newRelation terms clauses' sccAtoms'
    ]
  | otherwise =
    nestedSearchAndProject newRelation terms clauses sccAtoms -- TODO mempty?
  where
    newRelation = newRelationOf relation
    sccAtoms = clauses & filter isPartOfScc & mapMaybe (\case
      Atom _ name args -> Just (name, args)
      _ -> Nothing)
    sccClauseCount = length sccAtoms
    isPartOfScc = \case
      Atom _ name _  -> name `elem` sccNames
      _ -> False
    toDeltaClause = \case
      Atom nodeId clauseName args | clauseName `elem` sccNames ->
        Atom nodeId (deltaRelationOf clauseName) args
      clause -> clause

transformAt :: Int -> (a -> a) -> [a] -> [a]
transformAt i f =
  zipWith (\i' x -> if i == i' then f x else x) [0..]

-- TODO rm
-- recursiveRuleToStmts :: [Relation] -> Relation -> [CodegenM RA] -> [AST] -> CodegenM RA
-- recursiveRuleToStmts sccNames relation terms clauses =
--   nestedSearchAndProject relation newRelation terms clauses'
--   where
--     newRelation = newRelationOf relation
--     -- If there are multiple recursive clauses, convert (only?) the first to delta_RULE
--     clauses' =
--       if recursiveClauseCount > 1
--         then flip evalState False $ traverse f clauses
--         else clauses
--     f = \case
--       c@(Atom nodeId clauseName args) -> do
--         alreadyFound <- get
--         case (alreadyFound, clauseName == relation) of
--           (False, True) -> do
--             put True
--             let deltaClauseName = deltaRelationOf clauseName
--             pure $ Atom nodeId deltaClauseName args
--           _ ->
--             pure c
--       clause ->
--         pure clause
--     recursiveClauseCount = length $ filter isPartOfScc clauses
--     isPartOfScc = \case
--       Atom _ name _  -> name `elem` sccNames
--       _ -> False

nestedSearchAndProject
  :: Relation
  -> [CodegenM RA]
  -> [AST]
  -> [(Relation, [AST])]
  -> CodegenM RA
nestedSearchAndProject intoRelation terms clauses sccAtoms =
  flip (foldr processRuleClause) clauses $
    addNegatedDeltaAtoms sccAtoms $
      project intoRelation terms
  where
    processRuleClause :: AST -> CodegenM RA -> CodegenM RA
    processRuleClause clause inner = case clause of
      Not _ (Atom _ clauseName args) -> do
        -- No starts with check here, since cyclic negation is not allowed.
        let terms' = map toTerm args
        noElemOf clauseName terms' inner

      Constraint _ op lhs rhs -> do
        lhsTerm <- toTerm lhs
        rhsTerm <- toTerm rhs
        if' op lhsTerm rhsTerm inner

      Atom _ clauseName args -> do
        externs <- asks envExterns
        let isExtern = isJust $ find (\(Extern name _ _) -> clauseName == name) externs
        if isExtern
          then do
            clause' <- toTerm clause
            zero <- toTerm (Lit (NodeId 0) $ LNumber 0)
            if' NotEquals clause' zero inner
          else search clauseName args inner
      _ ->
        panic "Unexpected rule clause in 'nestedSearchAndProject'!"

    addNegatedDeltaAtoms =
      foldr (\(clauseName, args) wrapper -> wrapper . addNegatedDeltaAtom clauseName args) id

addNegatedDeltaAtom :: Relation -> [AST] -> CodegenM RA -> CodegenM RA
addNegatedDeltaAtom clauseName args =
  noElemOf (deltaRelationOf clauseName) (map toTerm args)

isRecursive :: [Relation] -> [AST] -> Bool
isRecursive sccNames clauses =
  let atomNames = flip mapMaybe clauses $ \case
        Atom _ name _ -> Just name
        _ -> Nothing
   in any (`elem` atomNames) sccNames

extractRuleData :: AST -> Maybe (Relation, [AST], [AST])
extractRuleData = \case
  Rule _ name args clauses -> Just (name, args, clauses)
  _ -> Nothing

newRelationOf, deltaRelationOf :: Relation -> Relation
deltaRelationOf = prependToId deltaPrefix
newRelationOf = prependToId newPrefix
