module Eclair.TypeSystem
  ( Type(..)
  , TypeError(..)
  , TypeInfo
  , typeCheck
  ) where

import Protolude hiding (Type, TypeError, fold)
import Control.Monad.Writer.Strict
import Data.Functor.Foldable
import Eclair.Syntax
import qualified Data.Map as Map


type TypeInfo = Map Id [Type]

-- NOTE: for now, no actual types are checked since everything is a u32.
data TypeError
  = UnknownAtom Id
  | ArgCountMismatch Id Int Int
  | DuplicateTypeDeclaration Id
  deriving Show


typeCheck :: AST -> Either [TypeError] TypeInfo
typeCheck ast
  | null errors = pure typeInfo
  | otherwise   = throwError errors
  where
    typeInfo = getTypeInfo ast
    errors = execWriter $ flip cata ast $ \case
      AtomF name args -> do
        case lookupType name of
          Nothing ->
            tell [UnknownAtom name]
          Just types ->
            checkArgCount name types args

      RuleF name args clauses -> do
        case lookupType name of
          Nothing ->
            tell [UnknownAtom name]

          Just types -> do
            checkArgCount name types args
            sequence_ clauses

      astf -> sequence_ astf

    lookupType name =
      Map.lookup name typeInfo

    checkArgCount name types args = do
      let actualArgCount = length args
          expectedArgCount = length types
      when (actualArgCount /= expectedArgCount) $ do
        tell [ArgCountMismatch name expectedArgCount actualArgCount]

-- TODO: check for duplicates
getTypeInfo :: AST -> TypeInfo
getTypeInfo ast =
  let typeDefs = extractTypeDefs ast
   in Map.fromList typeDefs

extractTypeDefs :: AST -> [(Id, [Type])]
extractTypeDefs = cata $ \case
  DeclareTypeF name tys -> [(name, tys)]
  AtomF {} -> mempty
  RuleF {} -> mempty
  astf -> foldMap identity astf
