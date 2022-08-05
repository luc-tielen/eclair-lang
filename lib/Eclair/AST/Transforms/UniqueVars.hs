module Eclair.AST.Transforms.UniqueVars
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
  Transform $ zygo renameVars rewrite
  where
    rewrite :: ASTF (StateT TransformState TransformM AST, TransformM AST) -> TransformM AST
    rewrite = \case
      RuleF nodeId name values clauses -> do
        vals <- traverse snd values
        (clauses', concatMap equalities -> assignClauses) <- unzip <$> traverse (usingStateT mempty . fst) clauses
        pure $ Rule nodeId name vals $ clauses' <> assignClauses
      astf ->
        embed <$> traverse snd astf

    renameVars :: RewriteRuleT (StateT TransformState) AST
    renameVars = \case
      AtomF nodeId name values ->
        Atom nodeId name <$> sequence values
      VarF nodeId v -> do
        let var = Var nodeId v
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
      astf ->
        embed <$> sequence astf


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
