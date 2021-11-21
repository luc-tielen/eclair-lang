module Eclair.TypeSystem
  ( Type(..)
  , TypeInfo
  , getTypeInfo
  ) where

import Protolude hiding (Type, fold)
import Data.Functor.Foldable
import Eclair.Syntax
import qualified Data.Map as Map

data Type
  = IntType
  deriving (Eq, Show)

type TypeInfo = Map Id [Type]

-- TODO: fix this hack by adding support for type declarations.
-- TODO: add proper typesystem / semantic analysis checks.
getTypeInfo :: AST -> TypeInfo
getTypeInfo = cata $ \case
  AtomF name args ->
    Map.singleton name $ toTypes args
  RuleF name args mappings ->
    Map.union (Map.singleton name $ toTypes args) $ combine mappings
  mappings ->
    combine mappings
  where
    combine :: Foldable f => f TypeInfo -> TypeInfo
    combine = foldr Map.union mempty
    -- Right now Eclair only supports integers.
    toTypes :: [a] -> [Type]
    toTypes = map (const IntType)
