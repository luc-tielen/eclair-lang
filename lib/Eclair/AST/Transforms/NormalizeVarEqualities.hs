module Eclair.AST.Transforms.NormalizeVarEqualities
  ( transform
  ) where

import Data.Functor.Foldable hiding (fold)
import qualified Data.Map as Map
import Data.List (partition)
import Eclair.Transform
import Eclair.AST.IR
import Eclair.Id


transform :: Transform AST AST
transform =
  Transform $ cata rewrite
  where
    rewrite :: RewriteRule AST
    rewrite = \case
      RuleF nodeId name values clauses -> do
        vals <- sequence values
        (clauses', equalities -> assignClauses) <- runStateT (traverse renameVars clauses) mempty
        pure $ Rule nodeId name vals $ clauses' <> assignClauses
      astf ->
        embed <$> sequence astf

    renameVars :: TransformM AST -> StateT TransformState TransformM AST
    renameVars m = lift m >>= \case
      -- TODO needs to be handled in 'rewrite' instead (using zygo?)
      Atom nodeId name values ->
        Atom nodeId name <$> traverse (renameVars . pure) values
      var@(Var nodeId v) -> do
        vs <- gets varMap
        case Map.lookup v vs of
          Nothing -> do
            modify $ \s -> s { varMap = Map.insert v 0 (varMap s) }
            pure var
          Just x -> do
            nodeId' <- lift freshNodeId
            let v' = Id $ "@" <> unId v <> "_" <> show x
                var' = Var nodeId v'  -- Same node id can be reused?
                eq = Assign nodeId' var var'
            modify $ \s -> s { equalities = eq : equalities s
                             , varMap = Map.insert v (x + 1) (varMap s)
                             }
            pure var'
      ast ->
        pure ast

data TransformState
  = TransformState
  { varMap :: Map Id Int
  , equalities :: [AST]
  }

instance Semigroup TransformState where
  (TransformState vm1 eqs1) <> (TransformState vm2 eqs2) =
    TransformState (Map.unionWith (+) vm1 vm2) (eqs1 <> eqs2)

instance Monoid TransformState where
  mempty = TransformState mempty mempty
