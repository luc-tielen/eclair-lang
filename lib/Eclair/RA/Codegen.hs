{-# LANGUAGE ViewPatterns, DerivingVia #-}

module Eclair.RA.Codegen
  ( CodegenM
  , runCodegen
  , emit
  , Term(..)
  , toTerm
  , project
  , search
  , loop
  , merge
  , swap
  , purge
  , exit
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Eclair.RA.IR
import Eclair.Syntax
import Protolude hiding (Constraint, swap)
import Protolude.Unsafe (unsafeFromJust, unsafeHead)
import qualified Data.Map as M


type Index = Int

-- TODO: can this be removed now?
data ConstraintType = Goal | Clause Int
  deriving (Eq, Ord, Show)

data Constraint = Constraint ConstraintType Relation Index Term
  deriving (Eq, Show)

-- TODO: keep track of clause nr inside codegen monad? not needed anymore?
data Constraints
  = Constraints [Constraint] (Map Id [Constraint])

instance Semigroup Constraints where
  (Constraints cs1 varAssocs1) <> (Constraints cs2 varAssocs2) =
    Constraints cs varAssocs
    where cs = cs1 <> cs2
          varAssocs = M.unionWith (<>) varAssocs1 varAssocs2

instance Monoid Constraints where
  mempty = Constraints mempty mempty

-- TODO: rename to Stmt/Statement?
newtype RADecl = RADecl { unRADecl :: RA }

newtype CodegenM a
  = CodeGenM (StateT [RADecl] (Reader Constraints) a)
  deriving ( Functor, Applicative, Monad
           , MonadReader Constraints, MonadState [RADecl])
  via (StateT [RADecl] (Reader Constraints))

runCodegen :: CodegenM a -> [RA]
runCodegen (CodeGenM m) =
  -- TODO: mempty is not correct; need constraints for rule args?
  -- TODO: how to process mutually recursive rules?
  -- idea: add helper function for scope/starting constraints?
  reverse $ map unRADecl $ runReader (execStateT m []) mempty

emit :: CodegenM RA -> CodegenM ()
emit m = do
  ra <- m
  modify (RADecl ra :)

data Term = VarTerm Id | LitTerm Number
  deriving (Eq, Show)

toTerm :: AST -> Term
toTerm (Lit x) = LitTerm x
toTerm (Var x) = VarTerm x
-- TODO fix, no catch-all
toTerm _ = panic "Unknown pattern in 'toTerm'"

project :: Relation -> [Term] -> CodegenM RA
project r ts =
  Project r <$> traverse resolveGoalTerm ts

search :: Relation -> [Term] -> CodegenM RA -> CodegenM RA
search r ts inner = do
  let depth = 0  -- TODO: increase clause index depth; not needed anymore?
  clauses <- traverse (uncurry (resolveClause r)) $ zip [0..] ts
  action <- local (addConstraints r depth $ zip [0..] ts) inner
  pure $ Search r (catMaybes clauses) action

loop :: [CodegenM RA] -> CodegenM RA
loop ms = Loop . Seq <$> sequence ms

merge :: Relation -> Relation -> CodegenM RA
merge from to = pure $ Merge from to

swap :: Relation -> Relation -> CodegenM RA
swap r1 r2 = pure $ Swap r1 r2

purge :: Relation -> CodegenM RA
purge r = pure $ Purge r

exit :: [Relation] -> CodegenM RA
exit rs = pure $ Exit rs

addConstraints :: Relation -> Int -> [(Int, Term)] -> Constraints -> Constraints
addConstraints r (Clause -> ty) ts cs = foldl' addConstraint cs ts where
  addConstraint :: Constraints -> (Int, Term) -> Constraints
  addConstraint (Constraints cs cMap) (i, t) = case t of
    VarTerm v ->
      let c = Constraint ty r i t
       in Constraints (c:cs) (M.insertWith (<>) v [c] cMap)
    LitTerm _ ->
      let c = Constraint ty r i t
       in Constraints (c:cs) cMap  -- TODO bug? cant update in map?? use just a list?

resolveGoalTerm :: Term -> CodegenM RA
resolveGoalTerm = \case
  LitTerm x -> pure $ RALit x
  VarTerm v -> do
    cs <- ask
    let (Constraint _ name idx _) = unsafeFromJust $ findBestMatchingConstraint cs v
     in pure $ ColumnIndex name idx

resolveTerm :: Term -> CodegenM RA
resolveTerm = \case
  LitTerm x -> pure $ RALit x
  VarTerm v -> do
    cs <- asks excludeGoals
    let (Constraint _ name idx _) = unsafeFromJust $ findBestMatchingConstraint cs v
     in pure $ ColumnIndex name idx

-- TODO: take Clause -> c into account
resolveClause :: Relation -> Index -> Term -> CodegenM (Maybe RA)
resolveClause r idx = \case
  LitTerm x -> pure $ Just $ RAConstraint lhs (RALit x)
  VarTerm v -> asks (lookupVar v) >>= \case
    Nothing -> pure Nothing
    Just cs -> do
      let relevantCs = sortOn descendingClauseIdx $ filter differentRelation cs
      case relevantCs of
        [] -> pure Nothing
        ((Constraint _ r' idx' _):_) ->
          pure $ Just $ RAConstraint lhs (ColumnIndex r' idx')
  where lhs = ColumnIndex r idx
        lookupVar v (Constraints _ cMap) = M.lookup v cMap
        descendingClauseIdx (Constraint ty _ _ _) = Down ty
        -- TODO: take clause idx into account here?:
        differentRelation (Constraint _ r' _ _) = r /= r'

excludeGoals :: Constraints -> Constraints
excludeGoals (Constraints cs cMap) = Constraints cs' cMap'
  where
    excludeGoal (Constraint ty _ _ _) = ty /= Goal
    excludeGoal' (filter excludeGoal -> cs')
      | null cs' = Nothing
      | otherwise = Just cs'
    cs' = filter excludeGoal cs
    cMap' = M.mapMaybe excludeGoal' cMap

findBestMatchingConstraint :: Constraints -> Id -> Maybe Constraint
findBestMatchingConstraint (Constraints _ cs) var =
  headMay . sortOn ascendingClauseIdx =<< M.lookup var cs
  where
    ascendingClauseIdx (Constraint ty _ _ _) = ty

{-
-- TODO: support other things than clauses... (gets rid of partial function)
clauseName :: AST -> Id
clauseName (Atom name _) = name
clauseName _ = panic "clauseName called in unsupported context"

-- TODO: support other things than clauses... (gets rid of partial function)
clauseConstraints :: [Constraint] -> AST -> [RA]
clauseConstraints eqs (Atom name values) =
  concat $ zipWith (constraintToRA name (excludeGoals eqs)) [0..] values
clauseConstraints _ _ = panic "clauseConstraints called in unsupported context"

constraintsForRule :: Relation -> [Value] -> [Clause] -> Constraints
constraintsForRule ruleName values clauses =
  let valueEqs = mkConstraints Goal ruleName values
      clauseEqs = concatMap (uncurry clauseToEqs) $ zip (Clause <$> [0..]) clauses
      mkConstraints ty name vals = zipWith (toConstraint ty name) [0..] vals
      clauseToEqs i = \case
        Atom name vals -> mkConstraints i name vals
        _ -> []  -- TODO fix/handle this
   in valueEqs <> clauseEqs
-}

