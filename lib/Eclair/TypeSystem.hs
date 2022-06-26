module Eclair.TypeSystem
  ( Type(..)
  , TypeError(..)
  , TypeInfo
  , typeCheck
  ) where

import qualified Data.List as List (head)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Functor.Foldable
import Eclair.AST.IR
import Eclair.Id


type TypeInfo = Map Id [Type]

-- NOTE: for now, no actual types are checked since everything is a u32.
data TypeError
  = UnknownAtom NodeId Id
  | ArgCountMismatch Id (NodeId, Int) (NodeId, Int)
  | DuplicateTypeDeclaration Id (NonEmpty (NodeId, [Type]))
  deriving (Eq, Ord, Show)


typeCheck :: AST -> Either [TypeError] TypeInfo
typeCheck ast
  | null errors = pure $ map snd typedefMap
  | otherwise   = throwError errors
  where
    errors = typeErrors ++ duplicateErrors typedefs

    typedefs = extractTypeDefs ast
    typedefMap = Map.fromList typedefs

    lookupType name =
      Map.lookup name typedefMap

    typeErrors = execWriter $ flip cata ast $ \case
      AtomF nodeId name args -> do
        case lookupType name of
          Nothing ->
            tell [UnknownAtom nodeId name]
          Just types ->
            checkArgCount name types (nodeId, args)

      RuleF nodeId name args clauses -> do
        case lookupType name of
          Nothing ->
            tell [UnknownAtom nodeId name]

          Just types -> do
            checkArgCount name types (nodeId, args)

        sequence_ clauses

      astf -> sequence_ astf

    checkArgCount name (nodeIdTypeDef, types) (nodeId, args) = do
      let actualArgCount = length args
          expectedArgCount = length types
      when (actualArgCount /= expectedArgCount) $ do
        tell [ArgCountMismatch name (nodeIdTypeDef, expectedArgCount) (nodeId, actualArgCount)]

duplicateErrors :: [(Id, (NodeId, [Type]))] -> [TypeError]
duplicateErrors typeDefs
  = sort typeDefs
  & groupBy ((==) `on` fst)
  & filter (\xs -> length xs /= 1)
  & map toDuplicateTypeDecl
  where
    toDuplicateTypeDecl :: NonEmpty (Id, (NodeId, [Type])) -> TypeError
    toDuplicateTypeDecl declInfo =
      let name = fst $ head declInfo
          decls = map snd declInfo
          sortedDecls = NE.sortBy (compare `on` fst) decls
       in DuplicateTypeDeclaration name sortedDecls

extractTypeDefs :: AST -> [(Id, (NodeId, [Type]))]
extractTypeDefs = cata $ \case
  DeclareTypeF nodeId name tys -> [(name, (nodeId, tys))]
  AtomF {} -> mempty
  RuleF {} -> mempty
  astf -> foldMap identity astf
