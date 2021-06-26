module Eclair ( run ) where

import Protolude
import Protolude.Unsafe (unsafeFromJust)
import Eclair.Syntax
import Eclair.Parser
import Eclair.RA.IR
import Control.Lens hiding (Equality, Index)
import qualified Data.Map as M
import qualified Data.Text as T


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
    -- TODO finish other cases
  processSingleRule name args clauses =
    -- TODO handle recursive rules that are not mutually recursive..
    let eqs = constraintsForRule name args clauses
        (c@(Atom cName values) : rest) = reverse clauses  -- TODO handle other cases (no atom at front)
        seed = Search cName (clauseConstraints eqs c) $
          Project name $ map (valueToRA eqs) args
     in foldl' (\raCode clause -> Search (clauseName clause) (clauseConstraints eqs clause) raCode) seed rest

-- TODO: support other things than clauses... (gets rid of partial function)
clauseName :: AST -> Id
clauseName (Atom name _) = name
clauseName _ = panic "clauseName called in unsupported context"

-- TODO: support other things than clauses... (gets rid of partial function)
clauseConstraints :: [Equality] -> AST -> [RA]
clauseConstraints eqs (Atom name values) =
  concat $ zipWith (constraintToRA name (excludeGoals eqs)) [0..] values
clauseConstraints _ _ = panic "clauseConstraints called in unsupported context"

constraintToRA :: Id -> [Equality] -> Int -> AST -> [RA]
constraintToRA name eqs i = \case
  Lit x -> [RAConstraint lhs $ RALit x]
  Var v ->
    let eqs' = sortOn ascendingClauseIdx $
          filter (\eq -> sameVar v eq && differentRelation eq) eqs
    in map g eqs'
  _ -> panic "Unexpected case in constraintToRA"
  where lhs = ColumnIndex name i
        sameVar var (Equality _ _ _ v) = VarTerm var == v
        differentRelation (Equality _ name' _ _) = name /= name'
        g (Equality _ name idx term) =
          case term of
            LitTerm lit -> RAConstraint lhs (RALit lit)
            VarTerm _ -> RAConstraint lhs (ColumnIndex name idx)

valueToRA :: [Equality] -> AST -> RA
valueToRA eqs = \case
  Lit x -> RALit x
  Var v ->
    let (Equality _ name idx _) = unsafeFromJust $ findBestMatchingEquality (excludeGoals eqs) v
      in ColumnIndex name idx
  _ -> panic "Unexpected case in argToRA"

ascendingClauseIdx :: Equality -> EqType
ascendingClauseIdx (Equality ty _ _ _) = ty

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
  = case resolved of
      LitTerm _ -> [Equality ty r i resolved]
      VarTerm _ -> filter (sameVar resolved) eqs
  | otherwise = []
  where
    matchingEquality (Equality ty r' i' _) =
      ty == Clause c && r == r' && i == i'
    sameVar resolved (Equality _ _ _ v) = resolved == v

excludeGoals :: [Equality] -> [Equality]
excludeGoals =
  filter (\(Equality ty _ _ _) -> ty /= Goal)

findBestMatchingEquality :: [Equality] -> Id -> Maybe Equality
findBestMatchingEquality eqs var =
  headMay $ sortOn ascendingClauseIdx $ filter sameVar eqs
  where
    ascendingClauseIdx (Equality ty _ _ _) = ty
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
  map compileRA <$> parseFile path

run :: FilePath -> IO ()
run path = compile path >>= \case
  Left err -> printParseError err
  Right ast -> print ast
