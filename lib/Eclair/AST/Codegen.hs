{-# LANGUAGE TupleSections, DerivingVia #-}

module Eclair.AST.Codegen
  ( CodegenM
  , runCodegen
  , emit
  , Term(..)
  , toTerm
  , ConstraintExpr(..)
  , Clause(..)
  , toClause
  , project
  , search
  , loop
  , parallel
  , merge
  , swap
  , purge
  , exit
  , noElemOf
  , if'
  ) where

import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Eclair.RA.IR as RA
import qualified Eclair.AST.IR as AST
import Eclair.Id


type Relation = RA.Relation
type RA = RA.RA

type Column = Int

newtype Row = Row { unRow :: Int }
  deriving (Eq, Ord)

type Variable = Id

data Constraint = Constraint Relation Row Column Variable
  deriving (Eq)

data Constraints
  = Constraints [Constraint] (Map Id [Constraint])

type ExtraConstraints = [ConstraintExpr]

newtype CodegenM a
  = CodeGenM (RWS (Row, Constraints) [RA] ExtraConstraints a)
  deriving ( Functor, Applicative, Monad
           , MonadReader (Row, Constraints)
           , MonadState ExtraConstraints
           , MonadWriter [RA]
           )
  via (RWS (Row, Constraints) [RA] ExtraConstraints)

runCodegen :: CodegenM a -> [RA]
runCodegen (CodeGenM m) =
  let cs = Constraints mempty mempty
   in snd $ execRWS m (Row 0, cs) []

emit :: CodegenM RA -> CodegenM ()
emit m = do
  ra <- m
  tell [ra]

data Term = VarTerm Id | LitTerm Word32
  deriving (Eq)

toTerm :: AST.AST -> Term
toTerm = \case
  AST.Lit _ lit ->
    case lit of
      AST.LNumber x ->
        LitTerm x
      AST.LString _ ->
        panic "Unexpected string literal in 'toTerm' when lowering to RA"
  AST.Var _ x ->
    VarTerm x
  -- TODO fix, no catch-all
  _ ->
    panic "Unknown pattern in 'toTerm'"

data ConstraintExpr
  = NotElem Id [Term]

noElemOf :: Relation -> [Term] -> CodegenM a -> CodegenM a
noElemOf r ts = constrain (NotElem r ts)

if' :: Term -> Term -> CodegenM RA -> CodegenM RA
if' lhsTerm rhsTerm body = do
  lhs <- resolveTerm lhsTerm
  rhs <- resolveTerm rhsTerm
  RA.If lhs rhs <$> body

constrain :: ConstraintExpr -> CodegenM a -> CodegenM a
constrain c m = do
  modify (c:)
  m

data Clause
  = AtomClause Id [Term]
  | ConstrainClause ConstraintExpr
  | AssignClause Term Term

toClause :: AST.AST -> Clause
toClause = \case
  AST.Atom _ name values ->
    AtomClause name (map toTerm values)
  AST.Assign _ lhs rhs ->
    AssignClause (toTerm lhs) (toTerm rhs)
  _ ->
    panic "toClause: unsupported case"

project :: Relation -> [Term] -> CodegenM RA
project r ts =
  RA.Project r <$> traverse resolveTerm ts

search :: Relation -> [Term] -> CodegenM RA -> CodegenM RA
search r ts inner = do
  row <- asks fst
  clauses <- traverse (uncurry (resolveClause r)) $ zip [0..] ts
  (action, extraClauses) <- local updateState $ do
    action <- inner
    (action,) <$> resolveExtraClauses
  let allClauses = catMaybes clauses ++ extraClauses
  pure $ RA.Search r (relationToAlias r row) allClauses action
  where
    updateState (row, cs) =
      let alias = relationToAlias r row
       in (incrRow row, addConstraints alias row (zip [0..] ts) cs)
    incrRow = Row . (+1) . unRow

relationToAlias :: Relation -> Row -> RA.Alias
relationToAlias r row =
  appendToId r (show $ unRow row)

loop :: [CodegenM RA] -> CodegenM RA
loop ms = RA.Loop <$> sequence ms

parallel :: [CodegenM RA] -> CodegenM RA
parallel ms = RA.Par <$> sequence ms

merge :: Relation -> Relation -> CodegenM RA
merge from' to' = pure $ RA.Merge from' to'

swap :: Relation -> Relation -> CodegenM RA
swap r1 r2 = pure $ RA.Swap r1 r2

purge :: Relation -> CodegenM RA
purge r = pure $ RA.Purge r

exit :: [Relation] -> CodegenM RA
exit rs = pure $ RA.Exit rs

addConstraints :: Relation -> Row -> [(Column, Term)] -> Constraints -> Constraints
addConstraints r row ts cs = foldl' addConstraint cs ts
  where
    addConstraint :: Constraints -> (Column, Term) -> Constraints
    addConstraint cs'@(Constraints cList cMap) (col, t) = case t of
      VarTerm v ->
        let c = Constraint r row col v
        in Constraints (c:cList) (M.insertWith (<>) v [c] cMap)
      LitTerm _ -> cs'

resolveTerm :: Term -> CodegenM RA
resolveTerm = \case
  LitTerm x -> pure $ RA.Lit x
  VarTerm v -> do
    cs <- asks snd
    let (Constraint name _ col _) = fromJust $ findBestMatchingConstraint cs v
     in pure $ RA.ColumnIndex name col

resolveClause :: Relation -> Column -> Term -> CodegenM (Maybe RA)
resolveClause r col t = do
  row <- asks fst
  let alias = relationToAlias r row
      lhs = RA.ColumnIndex alias col
  case t of
    LitTerm x -> pure $ Just $ RA.Constrain lhs (RA.Lit x)
    VarTerm v -> asks (lookupVar v . snd) >>= \case
      Nothing -> pure Nothing
      Just cs -> do
        let relevantCs = sortOn descendingClauseRow $ filter differentRelation cs
        case relevantCs of
          [] -> pure Nothing
          ((Constraint r' _ col' _):_) ->
            pure $ Just $ RA.Constrain lhs (RA.ColumnIndex r' col')
  where lookupVar v (Constraints _ cMap) = M.lookup v cMap
        descendingClauseRow (Constraint _ row _ _) = Down row
        differentRelation (Constraint r' _ _ _) = r /= r'

resolveExtraClauses :: CodegenM [RA]
resolveExtraClauses = do
  -- TODO: make algorithm smarter to take vars in use into account?
  extraClauses <- get
  modify (const [])
  traverse toRA extraClauses
  where
    toRA (NotElem r ts) = RA.NotElem r <$> traverse resolveTerm ts

findBestMatchingConstraint :: Constraints -> Id -> Maybe Constraint
findBestMatchingConstraint (Constraints _ cs) var =
  viaNonEmpty head . sortOn ascendingClauseRow =<< M.lookup var cs
  where ascendingClauseRow (Constraint _ row _ _) = row

