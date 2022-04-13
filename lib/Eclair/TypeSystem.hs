module Eclair.TypeSystem
  ( Type(..)
  , TypeError(..)
  , TypeInfo
  , typeCheck
  ) where

import Protolude hiding (Type, TypeError, fold, head)
import Data.List (head)
import Control.Monad.Writer.Strict
import Data.Functor.Foldable
import Eclair.AST.IR
import qualified Data.Map as Map


type TypeInfo = Map Id [Type]

-- NOTE: for now, no actual types are checked since everything is a u32.
data TypeError
  = UnknownAtom Id
  | ArgCountMismatch Id Int Int
  | DuplicateTypeDeclaration Id
  deriving (Eq, Ord, Show)


typeCheck :: AST -> Either [TypeError] TypeInfo
typeCheck ast
  | null errors = pure typeInfo
  | otherwise   = throwError errors
  where
    typeInfo = Map.fromList typeDefs
    errors = typeErrors ++ duplicateErrors
    duplicateErrors =
        sort typeDefs
      & groupBy ((==) `on` fst)
      & filter (\xs -> length xs /= 1)
      & map (DuplicateTypeDeclaration . fst . head)
    typeDefs = extractTypeDefs ast
    typeErrors = execWriter $ flip cata ast $ \case
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

extractTypeDefs :: AST -> [(Id, [Type])]
extractTypeDefs = cata $ \case
  DeclareTypeF name tys -> [(name, tys)]
  AtomF {} -> mempty
  RuleF {} -> mempty
  astf -> foldMap identity astf
