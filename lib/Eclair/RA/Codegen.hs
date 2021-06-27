{-# LANGUAGE ViewPatterns, DerivingVia #-}

module Eclair.RA.Codegen
  ( CodegenM
  , runCodegen
  , emit
  , Term(..)
  , toTerm
  , Clause(..)
  , toClause
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


-- TODO newtype?
type Column = Int

newtype Row = Row Int
  deriving (Eq, Ord, Show)

data Constraint = Constraint Relation Row Column Term
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

data Clause
  = AtomClause Id [Term]
  -- TODO add other variants later

toClause :: AST -> Clause
toClause = \case
  Atom name values -> AtomClause name (map toTerm values)
  _ -> panic "toClause: unsupported case"

project :: Relation -> [Term] -> CodegenM RA
project r ts =
  Project r <$> traverse resolveTerm ts

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
addConstraints r (Row -> row) ts cs = foldl' addConstraint cs ts where
  addConstraint :: Constraints -> (Int, Term) -> Constraints
  addConstraint (Constraints cs cMap) (i, t) = case t of
    VarTerm v ->
      let c = Constraint r row i t
       in Constraints (c:cs) (M.insertWith (<>) v [c] cMap)
    LitTerm _ ->
      let c = Constraint r row i t
       in Constraints (c:cs) cMap  -- TODO bug? cant update in map?? use just a list?

resolveTerm :: Term -> CodegenM RA
resolveTerm = \case
  LitTerm x -> pure $ RALit x
  VarTerm v -> do
    cs <- ask
    let (Constraint name _ idx _) = unsafeFromJust $ findBestMatchingConstraint cs v
     in pure $ ColumnIndex name idx

-- TODO: take Clause -> c into account
resolveClause :: Relation -> Column -> Term -> CodegenM (Maybe RA)
resolveClause r idx = \case
  LitTerm x -> pure $ Just $ RAConstraint lhs (RALit x)
  VarTerm v -> asks (lookupVar v) >>= \case
    Nothing -> pure Nothing
    Just cs -> do
      let relevantCs = sortOn descendingClauseIdx $ filter differentRelation cs
      case relevantCs of
        [] -> pure Nothing
        ((Constraint r' _ idx' _):_) ->
          pure $ Just $ RAConstraint lhs (ColumnIndex r' idx')
  where lhs = ColumnIndex r idx
        lookupVar v (Constraints _ cMap) = M.lookup v cMap
        descendingClauseIdx (Constraint ty _ _ _) = Down ty
        -- TODO: take clause idx into account here?:
        differentRelation (Constraint r' _ _ _) = r /= r'

findBestMatchingConstraint :: Constraints -> Id -> Maybe Constraint
findBestMatchingConstraint (Constraints x cs) var =
  traceShow x $ headMay . sortOn ascendingClauseIdx =<< M.lookup var cs
  where
    ascendingClauseIdx (Constraint ty _ _ _) = ty

