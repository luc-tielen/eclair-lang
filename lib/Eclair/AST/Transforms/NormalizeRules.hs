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
  Transform $ zygo normalizeArithmetic rewriteArithmetic
          >=> zygo normalizeEqualVars rewriteVars
  where
    rewriteVars = \case
      RuleF nodeId name values clauses -> do
        vals <- traverse snd values
        let beginState = NormalizeVarState mempty mempty
        (clauses', extras) <- unzip <$> traverse (usingStateT beginState . fst) clauses
        let (constraints, rest) = partition isConstraint clauses'
            extras' = concatMap extraClauses extras
        pure $ Rule nodeId name vals $ rest <> constraints <> extras'
      astf ->
        embed <$> traverse snd astf

    normalizeEqualVars = \case
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
      astf ->
        embed <$> sequence astf

    isConstraint :: AST -> Bool
    isConstraint = \case
      Constraint {} -> True
      _ -> False

    normalizeArithmetic = \case
      BinOpF nodeId op lhs rhs -> do
        count <- gets nextVar
        modify $ \s -> s { nextVar = nextVar s + 1 }
        binOp <- BinOp nodeId op <$> lhs <*> rhs
        nodeId' <- lift freshNodeId
        let v = Id $ "@binop_" <> show count
            var = Var nodeId' v
            eq = Constraint nodeId' Equals var binOp
        modify $ \s -> s { extraArithClauses = eq : extraArithClauses s }
        pure var
      astf ->
        embed <$> sequence astf

    rewriteArithmetic = \case
      RuleF nodeId name values clauses -> do
        let beginState = NormalizeArithmeticState 0 mempty
        ((values', clauses'), endState) <- usingStateT beginState $ do
            vals <- traverse fst values
            cs <- traverse fst clauses
            pure (vals, cs)
        let clauses'' = clauses' <> extraArithClauses endState
        pure $ Rule nodeId name values' clauses''
      astf ->
        embed <$> traverse snd astf

data NormalizeVarState
  = NormalizeVarState
  { varMap :: Map Id Int
  , extraClauses :: [Clause]
  }

data NormalizeArithmeticState
  = NormalizeArithmeticState
  { nextVar :: Int
  , extraArithClauses :: [Clause]
  }
