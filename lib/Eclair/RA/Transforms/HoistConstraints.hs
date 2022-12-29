module Eclair.RA.Transforms.HoistConstraints
  ( transform
  ) where

import Eclair.Transform
import Eclair.RA.IR
import Eclair.Comonads
import Eclair.Common.Id
import Eclair.Common.Location (NodeId(..))
import Data.List (partition)


data HoistState
  = HoistState
  { seenAliases :: [Id]
  , remainingConstraints :: [(NodeId, [Id], RA)]
  }

transform :: Transform RA RA
transform =
  let beginState = HoistState mempty mempty
   in Transform $ usingReaderT beginState . rewrite
  where
    rewrite = gcata (distribute collectAliases collectConstraints) hoistConstraints

    distribute :: Corecursive t
               => (Base t a -> a)
               -> (Base t (t, a, b) -> b)
               -> (Base t (Quad t a b c) -> Quad t a b (Base t c))
    distribute f g m =
      let base_t_t = map qFirst m
          base_t_a = map qSecond m
          base_t_tab = map (\q -> (qFirst q, qSecond q, qThird q)) m
          base_t_c = map qFourth m
       in Quad (embed base_t_t) (f base_t_a) (g base_t_tab) base_t_c

    collectAliases = \case
      ColumnIndexF _ alias _ -> one alias
      raf -> fold raf

    collectConstraints = \case
      IfF nodeId condition inner -> do
        let cond = getCondition nodeId condition
        cond : getValues inner
      raf ->
        foldMap getValues raf
      where
        getCondition nodeId (ra, aliases, _) = (nodeId, aliases, ra)
        getValues (_, _, values) = values

    hoistConstraints = \case
      SearchF nodeId r a cs inner -> do
        isFirstSearch <- asks (null . seenAliases)
        let innerConstraints = qThird inner
            withUpdatedEnv = local $
              if isFirstSearch
                then const $ HoistState [a] innerConstraints
                else \s -> s { seenAliases = a : seenAliases s }

        withUpdatedEnv $ do
          HoistState aliases constraints <- ask
          let (covered, rest) = partition (coversAllAliases aliases) constraints
              (indexable, nonIndexable) = partition supportsIndex covered
              cs' = map qFirst cs <> map (\(_, _, c) -> c) indexable
              addNonIndexableConstraints =
                nonIndexable
                & map (\(nodeId', _, cond) -> If nodeId' cond)
                & foldl' (.) id

          local (\s -> s { remainingConstraints = rest }) $ do
            Search nodeId r a cs' . addNonIndexableConstraints <$> qFourth inner

      ProjectF nodeId r vals -> do
        -- NOTE: remaining conditions are not removed here, to support multiple projections in the future.
        remaining <- asks (map (\(nodeId', _, cond) -> (nodeId', cond)) . remainingConstraints)
        projectStmt <- Project nodeId r <$> traverse qFourth vals
        pure $ foldr (\(nodeId', cond) inner -> If nodeId' cond inner) projectStmt remaining

      IfF _ _ inner ->
        -- NOTE: constraints are already in the state and handled in
        -- project and search, so we don't do anything here.
        qFourth inner

      raf ->
        embed <$> traverse qFourth raf

    coversAllAliases aliases (_, as, _) =
      all (`elem` aliases) as

    supportsIndex = \case
      (_, _, ra) -> isIndexable ra

    -- TODO add pass for RA so <= and >= can be used in an index
    isIndexable = \case
      CompareOp _ op (ColumnIndex _ a1 _) (ColumnIndex _ a2 _) ->
        op == Equals && a1 /= a2
      CompareOp _ op _ _ ->
        op == Equals
      _ ->
        False
