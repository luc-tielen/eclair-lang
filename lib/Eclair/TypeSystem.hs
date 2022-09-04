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
import Data.List ((!!), partition)
import Data.Maybe (fromJust)

-- NOTE: This module contains a lot of partial functions due to the fact
-- that the rest of the compiler relies heavily on recursion-schemes.
-- This is however one place where I have not figured out how to apply a
-- recursion-scheme here.. yet!


type TypeInfo = Map Id [Type]

-- NOTE: for now, no actual types are checked since everything is a u32.
data TypeError
  = UnknownAtom NodeId Id
  | ArgCountMismatch Id (NodeId, Int) (NodeId, Int)
  | DuplicateTypeDeclaration Id (NonEmpty (NodeId, [Type]))
  | TypeMismatch NodeId Type Type  -- 1st type is actual, 2nd is expected
                                   -- TODO add context to keep track of how this was deduced?
  deriving (Eq, Ord, Show)


typeCheck :: AST -> Either [TypeError] TypeInfo
typeCheck ast
  | null errors = pure $ map snd typedefMap
  | otherwise   = throwError errors
  where
    errors = checkDecls typedefMap ast ++ duplicateErrors typedefs

    typedefs = extractTypeDefs ast
    typedefMap = Map.fromList typedefs

type TypedefMap = Map Id (NodeId, [Type])

data TCState
  = TCState
  { typedefs :: TypedefMap
  , typeEnv :: Map Id Type
  , errors :: [TypeError]
  }

-- TODO add context?
type TypeCheckM = State TCState

runM :: TypedefMap -> TypeCheckM a -> [TypeError]
runM typedefMap m =
  errors $ execState m (TCState typedefMap mempty mempty)

lookupType :: Id -> TypeCheckM (Maybe (NodeId, [Type]))
lookupType name = do
  typedefMap <- gets typedefs
  pure $ Map.lookup name typedefMap

-- TODO: rename to unifyVar?
bindVar :: Id -> Type -> TypeCheckM ()
bindVar var ty =
  modify' $ \s -> s { typeEnv = Map.insert var ty (typeEnv s) }

emitError :: TypeError -> TypeCheckM ()
emitError err =
  modify' $ \s -> s { errors = err : errors s }


-- TODO: try to merge with checkDecl?
checkDecls :: TypedefMap -> AST -> [TypeError]
checkDecls typedefMap = \case
  Module _ decls ->
    let state = TCState typedefMap mempty mempty
     in concatMap (runM typedefMap . checkDecl) decls
  _ ->
    panic "Unexpected AST node in 'checkDecls'"

checkDecl :: AST -> TypeCheckM ()
checkDecl = \case
  DeclareType {} ->
    pass
  Atom nodeId name args ->
    lookupType name >>= \case
      Nothing ->
        emitError $ UnknownAtom nodeId name
      Just (nodeId', types) -> do
        checkArgCount name (nodeId', types) (nodeId, args)
        zipWithM_ checkExpr args types
  Rule nodeId name args clauses ->
    lookupType name >>= \case
      Nothing ->
        emitError $ UnknownAtom nodeId name
      Just (nodeId', types) -> do
        checkArgCount name (nodeId', types) (nodeId, args)
        zipWithM_ checkExpr args types

        let (assignClauses, restClauses) = partition isAssign clauses
            clauses' = restClauses ++ assignClauses
        traverse_ checkDecl clauses'
  _ ->
    panic "Unexpected case in 'checkDecl'"
  where
    isAssign = \case
      Assign {} -> True
      _ -> False

checkArgCount :: Id -> (NodeId, [Type]) -> (NodeId, [AST]) -> TypeCheckM ()
checkArgCount name (nodeIdTypeDef, types) (nodeId, args) = do
  let actualArgCount = length args
      expectedArgCount = length types
  when (actualArgCount /= expectedArgCount) $
    emitError $ ArgCountMismatch name (nodeIdTypeDef, expectedArgCount) (nodeId, actualArgCount)

checkExpr :: AST -> Type -> TypeCheckM ()
checkExpr ast expectedTy = case ast of
  l@(Lit nodeId _) -> do
    actualTy <- inferExpr l
    when (actualTy /= expectedTy) $
      emitError $ TypeMismatch nodeId actualTy expectedTy
  PWildcard {} ->
    -- NOTE: no checking happens for wildcards, since they always refer to
    -- unique vars (and thus cannot cause type errors)
    pass
  Var nodeId var -> do
    gets (Map.lookup var . typeEnv) >>= \case
      Nothing ->
        -- TODO: also store in context a variable was bound here for better errors?
        bindVar var expectedTy
      Just actualTy -> do
        when (actualTy /= expectedTy) $
          emitError $ TypeMismatch nodeId actualTy expectedTy
  Assign nodeId lhs rhs -> do
    lhsTy <- inferExpr lhs
    rhsTy <- inferExpr rhs
    when (lhsTy /= rhsTy) $
      -- TODO better error?
      emitError $ TypeMismatch nodeId lhsTy rhsTy
  _ ->
    panic "Unexpected case in 'checkExpr'"

inferExpr :: AST -> TypeCheckM Type
inferExpr = \case
  Lit _ lit ->
    case lit of
      LNumber {} ->
        pure U32
      LString {} ->
        pure Str
  Var _ var -> do
    gets (Map.lookup var . typeEnv) >>= \case
      Nothing ->
        -- NOTE: should be impossible if all assigns are moved to end of rule body
        -- though this probably needs to be made smarter when we come across more complicated statements.
        panic "Variable with unknown type in 'inferExpr'"
      Just ty ->
        pure ty
  _ ->
    panic "Unexpected case in 'inferExpr'"

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
