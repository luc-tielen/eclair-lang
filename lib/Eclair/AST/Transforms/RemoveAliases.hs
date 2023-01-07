module Eclair.AST.Transforms.RemoveAliases
  ( transform
  ) where

import qualified Data.Map as M
import Eclair.Transform
import Eclair.AST.IR
import Eclair.Comonads
import Eclair.Common.Extern

-- This transform reduces the amount of helper variables used in assignments.

transform :: [Extern] -> Transform AST AST
transform externs =
  Transform $ usingReaderT Nothing . gcata (distribute directlyGroundedVars equatedVars) rewrite
  where
    distribute :: Corecursive t
               => (Base t (t, a) -> a)
               -> (Base t (t, b) -> b)
               -> Base t (Quad t a b c) -> Quad t a b (Base t c)
    distribute f g m =
      let base_t_t = map qFirst m
          base_t_ta = map (qFirst &&& qSecond) m
          base_t_tb = map (qFirst &&& qThird) m
          base_t_c = map qFourth m
       in Quad (embed base_t_t) (f base_t_ta) (g base_t_tb) base_t_c

    externNames = map (\(Extern name _ _) -> name) externs

    -- Finds all vars directly inside a (not externally defined) atom.
    directlyGroundedVars = \case
      AtomF _ name args | name `notElem` externNames ->
        flip mapMaybe args $ \case
          (Var _ v, _) -> Just v
          _ -> Nothing
      astf ->
        foldMap snd astf

    -- Find all vars used in equalities
    equatedVars = \case
      ConstraintF _ Equals lhs rhs -> case (fst lhs, fst rhs) of
        (lhs'@(Var _ v1), rhs'@(Var _ v2)) ->
          [(v1, rhs'), (v2, lhs')]
        (Var _ v, rhs') ->
          one (v, rhs')
        (lhs', Var _ v) ->
          one (v, lhs')
        _ ->
          mempty
      astf ->
        foldMap snd astf

    -- Aliases = equated vars - directly grounded vars
    findAliases dgVars = filter ((`notElem` dgVars) . fst)

    rewrite = \case
      RuleF nodeId name args clauses -> do
        let dgVars = concatMap qSecond clauses
            eqs = concatMap qThird clauses
            subst = M.fromList $ filter (not . occursCheck) $ findAliases dgVars eqs
        local (const $ Just subst) $
          Rule nodeId name
            <$> traverse extract args
            <*> traverse extract clauses
      VarF nodeId v -> do
        let var = Var nodeId v
        -- Because of how the substitution is constructed,
        -- it's always safe to try and replace variables.
        maybe var (`resolveAliases` var) <$> ask
      astf ->
        embed <$> traverse extract astf

    resolveAliases subst =
      ana $ \case
        Var nodeId v ->
          maybe (VarF nodeId v) project $ M.lookup v subst
        ast -> project ast

    -- Occurs check is needed to prevent aliases from growing larger and larger.
    occursCheck (v, ast) =
      let vars = flip cata ast $ \case
            VarF _ var -> [var]
            astf -> fold astf
      in v `elem` vars
