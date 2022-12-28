module Eclair.AST.Transforms.NormalizeRules
  ( transform
  ) where

import Data.List (partition)
import qualified Data.Map as Map
import Eclair.Transform
import Eclair.AST.IR
import Eclair.Common.Id
import Eclair.Comonads

-- This transform does a bunch of things to prepare for lowering to RA:
-- 1. It replaces equal variables in one atom with unique variables,
--    producing extra assign constraints along the way;
-- 2. It shifts all constraints to the end;
-- 3. It shifts all arithmetic used in clauses to the end.

transform :: Transform AST AST
transform =
  Transform $ gcata (distribute normalizeVars) rewriteVars
  where
    distribute
      :: Corecursive t
      => (Base t (t, a) -> a)
      -> (Base t (Triple t a b) -> Triple t a (Base t b))
    distribute g m =
      let base_t_t = map tFst m
          base_t_ta = map (tFst &&& tSnd) m
          base_t_b = map tThd m
       in Triple (embed base_t_t) (g base_t_ta) base_t_b

    rewriteVars = \case
      RuleF nodeId name values clauses -> do
        vals <- traverse extract values
        let beginState = NormalizeVarState mempty mempty
        (clauses', extras) <- unzip <$> traverse (usingStateT beginState . tSnd) clauses
        let (constraints, rest) = partition isConstraint clauses'
            extras' = concatMap extraVarClauses extras
        pure $ Rule nodeId name vals $ rest <> constraints <> extras'
      astf ->
        embed <$> traverse extract astf

    normalizeVars = \case
      AtomF nodeId name values ->
        Atom nodeId name <$> traverse snd values
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
            modify $ \s -> s { extraVarClauses = eq : extraVarClauses s
                             , varMap = Map.insert v (x + 1) (varMap s)
                             }
            pure var'
      -- Don't rewrite vars in exprs, to avoid introducing ungrounded variables!
      BinOpF nodeId op lhs rhs -> do
        pure $ BinOp nodeId op (fst lhs) (fst rhs)
      astf ->
        embed <$> traverse snd astf

    isConstraint :: AST -> Bool
    isConstraint = \case
      Constraint {} -> True
      _ -> False

data NormalizeVarState
  = NormalizeVarState
  { varMap :: Map Id Int
  , extraVarClauses :: [Clause]
  }
