module Eclair ( run ) where

import Protolude
import Protolude.Unsafe (unsafeFromJust)
import Eclair.Parser
import Control.Lens hiding (Equality, Index)
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Text as T


type Relation = Id
type RAClause = RA
type Action = RA
type ColumnIndex = Int

-- NOTE: removed Insert, couldn't find a use?
data RA
  = Search Relation [RAClause] Action
  | Project Relation [RA]
  | Merge Relation Relation
  | Swap Relation Relation
  | Purge Relation
  | Seq RA RA
  -- | Par [RA]
  | Loop RA
  | Exit RAClause

  | RAModule [RA]
  | RALit Number
  | ColumnIndex Relation ColumnIndex
  | RAConstraint RA RA  -- TODO simplify names
  deriving (Eq, Show)

scc :: AST -> [[AST]]
scc = \case
  Module decls -> map G.flattenSCC sortedDecls where
    -- TODO: fix issue when loose atom does not appear
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
  _ -> panic "Unreachable code in 'scc'"


compileRA :: AST -> RA
compileRA ast = RAModule $ map processDecls sortedDecls where
  sortedDecls = scc ast

  processDecls :: [AST] -> RA
  processDecls = \case
    [Atom name values] ->
      Project name (RALit <$> values ^.. folded . _Lit)
    [Rule name args clauses] ->
      processSingleRule name args clauses
  -- TODO: which other cases?
    rules -> RAModule []
  processSingleRule name args clauses =
    -- TODO handle recursive rules..
    let eqs = constraintsForRule name args clauses
        (Atom cName values : rest) = reverse clauses  -- TODO handle other cases (no atom at front)
        seed = Project name $ map (valueToRA eqs) args
        -- TODO: support other things than clauses... (gets rid of partial functions!)
        clauseName (Atom name _) = name
        clauseName _ = panic "clauseName called in unsupported context"
        clauseConstraints (Atom name values) =
          concat $ zipWith (constraintToRA name (excludeGoals eqs)) [0..] values
        clauseConstraints _ = panic "clauseConstraints called in unsupported context"
     in foldl' (\raCode clause -> Search (clauseName clause) (clauseConstraints clause) raCode) seed rest
  constraintToRA name eqs i =
    let lhs = ColumnIndex name i
     in \case
        Lit x -> [RAConstraint lhs $ RALit x]
        Var v ->
          let eqs' = f v
              f var =
                sortOn ascendingClauseIdx $
                  filter (\eq -> sameVar eq && differentRelation eq) eqs
                where ascendingClauseIdx (Equality ty _ _ _) = ty
                      sameVar (Equality _ _ _ v) = VarTerm var == v
                      differentRelation (Equality _ name' _ _) = name /= name'
              g (Equality _ name idx term) =
                case term of
                  LitTerm lit -> RAConstraint lhs (RALit lit)
                  VarTerm _ -> RAConstraint lhs (ColumnIndex name idx)
          in map g eqs' --  [ColumnIndex name idx]
        _ -> panic "Unexpected case in constraintToRA"
  isLit = \case
    LitTerm _ -> True
    _ -> False
  valueToRA eqs = \case
    Lit x -> RALit x
    Var v ->
      let (Equality _ name idx _) = unsafeFromJust $ findBestMatchingEquality (excludeGoals eqs) v
       in ColumnIndex name idx
    _ -> panic "Unexpected case in argToRA"
  -- TODO finish other cases

type Index = Int

data Term = VarTerm Id | LitTerm Number
  deriving (Eq, Show)

-- TODO better names
data EqType = Goal | Clause Int
  deriving (Eq, Ord, Show)

-- TODO: rename to Constraint
data Equality = Equality EqType Relation Index Term
  deriving (Eq, Show)

-- combine with Map (Relation, Index) Term for perf?
type Equalities = [Equality]

-- Finds all equalities for a given value in an atom.
resolve :: Equalities -> Int -> Relation -> Index -> [Equality]
resolve eqs c r i
  | Just (Equality ty _ _ resolved) <- find matchingEquality eqs
  = if isLit resolved
      then [Equality ty r i resolved]
      else filter (sameVar resolved) eqs
  | otherwise = []
  where
    matchingEquality (Equality ty r' i' _) =
      ty == Clause c && r == r' && i == i'
    sameVar resolved (Equality _ _ _ v) = resolved == v
    isLit = \case
      LitTerm _ -> True
      _ -> False

excludeGoals :: [Equality] -> [Equality]
excludeGoals =
  filter (\(Equality ty _ _ _) -> ty /= Goal)

findBestMatchingEquality :: [Equality] -> Id -> Maybe Equality
findBestMatchingEquality eqs var =
  headMay $ sortOn ascendingClauseIdx $ filter sameVar eqs
  where ascendingClauseIdx (Equality ty _ _ _) = ty
        sameVar (Equality _ _ _ v) = VarTerm var == v

constraintsForRule :: Relation -> [Value] -> [Clause] -> Equalities
constraintsForRule ruleName values clauses =
  let valueEqs = mkEqualities Goal ruleName values
      clauseEqs = concatMap (uncurry clauseToEqs) $ zip (Clause <$> [0..]) clauses
      mkEqualities ty name vals = zipWith (toEquality ty name) [0..] vals
      clauseToEqs i = \case
        Atom name vals -> mkEqualities i name vals
        _ -> []  -- TODO fix/handle this
   in valueEqs <> clauseEqs

toEquality :: EqType -> Relation -> Index -> Value -> Equality
toEquality ty name i v = Equality ty name i (toTerm v) where
  toTerm (Lit x) = LitTerm x
  toTerm (Var x) = VarTerm x
  -- TODO fix, no catch-all
  toTerm _ = panic "Unknown pattern in 'toTerm'"

compile :: FilePath -> IO (Either ParseError RA)
compile path = do
  result <- parseFile path
  case result of
    Left err -> pure $ Left err
    Right ast -> do
      let ra = compileRA ast
      pure $ Right ra

run :: FilePath -> IO ()
run path = compile path >>= \case
  Left err -> printParseError err
  Right ast -> print ast
