module Eclair.AST.Transforms.NormalizeRules
  ( transform
  ) where

import Data.List (partition)
import qualified Data.Map as Map
import Eclair.Transform
import Eclair.AST.IR
import Eclair.Common.Id

-- This transform does a bunch of things to prepare for lowering to RA:
-- 1. It replaces equal variables in one atom with unique variables,
--    producing extra assign constraints along the way;
-- 2. It shifts all constraints to the end;
-- 3. It shifts all arithmetic to the end.

transform :: Transform AST AST
transform =
  Transform $ zygo normalize rewrite
  where
    rewrite :: ASTF (StateT TransformState TransformM AST, TransformM AST) -> TransformM AST
    rewrite = \case
      RuleF nodeId name values clauses -> do
        vals <- traverse snd values
        (clauses', concatMap extraClauses -> extras) <- unzip <$> traverse (usingStateT mempty . fst) clauses
        let (constraints, rest) = partition isConstraint clauses'
        pure $ Rule nodeId name vals $ rest <> constraints <> extras
      astf ->
        embed <$> traverse snd astf

    normalize :: RewriteRuleT (StateT TransformState) AST
    normalize = \case
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
                eq = Constraint nodeId' Equals var var'
            modify $ \s -> s { extraClauses = eq : extraClauses s
                             , varMap = Map.insert v (x + 1) (varMap s)
                             }
            pure var'
      BinOpF nodeId op lhs rhs -> do
        binOp <- BinOp nodeId op <$> lhs <*> rhs
        nodeId' <- lift freshNodeId
        count <- gets nextVar
        let v = Id $ "@binop_" <> show count
            var = Var nodeId' v
            eq = Constraint nodeId' Equals var binOp
        modify $ \s -> s { extraClauses = eq : extraClauses s
                         , nextVar = nextVar s + 1
                         }
        pure var
      astf ->
        embed <$> sequence astf

    isConstraint :: AST -> Bool
    isConstraint = \case
      Constraint {} -> True
      _ -> False


data TransformState
  = TransformState
  { varMap :: Map Id Int
  , extraClauses :: [Clause]
  , nextVar :: Int
  }

instance Semigroup TransformState where
  (TransformState vm1 eqs1 nextVar1) <> (TransformState vm2 eqs2 nextVar2) =
    TransformState
      (Map.unionWith (+) vm1 vm2)
      (eqs1 <> eqs2)
      (nextVar1 + nextVar2)

instance Monoid TransformState where
  mempty = TransformState mempty mempty 0
