{-# LANGUAGE DerivingVia #-}

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
import qualified Data.Text as T


-- TODO newtype?
type Column = Int

newtype Row = Row { unRow :: Int }
  deriving (Eq, Ord, Show)

type Variable = Id

data Constraint = Constraint Relation Row Column Variable
  deriving (Eq, Show)

data Constraints
  = Constraints [Constraint] (Map Id [Constraint])

-- TODO RWST
newtype CodegenM a
  = CodeGenM (StateT [RA] (Reader (Row, Constraints)) a)
  deriving ( Functor, Applicative, Monad
           , MonadReader (Row, Constraints), MonadState [RA])
  via (StateT [RA] (Reader (Row, Constraints)))

runCodegen :: CodegenM a -> [RA]
runCodegen (CodeGenM m) =
  let cs = Constraints mempty mempty
   in reverse $ runReader (execStateT m []) (Row 0, cs)

emit :: CodegenM RA -> CodegenM ()
emit m = do
  ra <- m
  modify (ra :)

data Term = VarTerm Id | LitTerm Number
  deriving (Eq, Show)

toTerm :: AST -> Term
toTerm = \case
  Lit x -> LitTerm x
  Var x -> VarTerm x
  -- TODO fix, no catch-all
  _ -> panic "Unknown pattern in 'toTerm'"

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
  clauses <- traverse (uncurry (resolveClause r)) $ zip [0..] ts
  action <- local updateState inner
  row <- asks fst
  pure $ Search r (relationToAlias r row) (catMaybes clauses) action
  where
    updateState (row, cs) =
      let alias = relationToAlias r row
       in (incrRow row, addConstraints alias row (zip [0..] ts) cs)
    incrRow = Row . (+1) . unRow

relationToAlias :: Relation -> Row -> Alias
relationToAlias r row =
  appendToId r (T.pack . show $ unRow row)

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

addConstraints :: Relation -> Row -> [(Column, Term)] -> Constraints -> Constraints
addConstraints r row ts cs = foldl' addConstraint cs ts
  where
    addConstraint :: Constraints -> (Column, Term) -> Constraints
    addConstraint cs@(Constraints cList cMap) (col, t) = case t of
      VarTerm v ->
        let c = Constraint r row col v
        in Constraints (c:cList) (M.insertWith (<>) v [c] cMap)
      LitTerm _ -> cs

resolveTerm :: Term -> CodegenM RA
resolveTerm = \case
  LitTerm x -> pure $ RALit x
  VarTerm v -> do
    cs <- asks snd
    let (Constraint name _ col _) = unsafeFromJust $ findBestMatchingConstraint cs v
     in pure $ ColumnIndex name col

resolveClause :: Relation -> Column -> Term -> CodegenM (Maybe RA)
resolveClause r col t = do
  row <- asks fst
  let alias = relationToAlias r row
      lhs = ColumnIndex alias col
  case t of
    LitTerm x -> pure $ Just $ RAConstraint lhs (RALit x)
    VarTerm v -> asks (lookupVar v . snd) >>= \case
      Nothing -> pure Nothing
      Just cs -> do
        let relevantCs = sortOn descendingClauseRow $ filter differentRelation cs
        case relevantCs of
          [] -> pure Nothing
          ((Constraint r' _ col' _):_) ->
            pure $ Just $ RAConstraint lhs (ColumnIndex r' col')
  where lookupVar v (Constraints _ cMap) = M.lookup v cMap
        descendingClauseRow (Constraint _ row _ _) = Down row
        -- TODO: take clause idx into account here?:
        differentRelation (Constraint r' _ _ _) = r /= r'

findBestMatchingConstraint :: Constraints -> Id -> Maybe Constraint
findBestMatchingConstraint (Constraints x cs) var =
  headMay . sortOn ascendingClauseRow =<< M.lookup var cs
  where
    ascendingClauseRow (Constraint _ row _ _) = row

