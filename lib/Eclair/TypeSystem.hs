module Eclair.TypeSystem
  ( Type(..)
  , TypeError(..)
  , TypeInfo
  , typeCheck
  ) where

import qualified Data.List as List (head)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
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
  | UnificationFailure Type Type   -- No node id here? so need to get it from context?
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

data CheckState
  = CheckState
  { typedefs :: TypedefMap
  , typeEnv :: Map Id Type  -- NOTE: should not contain unification variables (TUnknown)!
  , substitution :: IntMap Type
  , nextVar :: Int
  , errors :: [TypeError]
  }

-- TODO add context for error reporting
type TypeCheckM = State CheckState

runM :: TypedefMap -> TypeCheckM a -> [TypeError]
runM typedefMap m =
  errors $ execState m (CheckState typedefMap mempty mempty 0 mempty)

emitError :: TypeError -> TypeCheckM ()
emitError err =
  modify $ \s -> s { errors = err : errors s }

bindVar :: Id -> Type -> TypeCheckM ()
bindVar var ty =
  modify $ \s -> s { typeEnv = Map.insert var ty (typeEnv s) }

lookupRelationType :: Id -> TypeCheckM (Maybe (NodeId, [Type]))
lookupRelationType name =
  gets (Map.lookup name . typedefs)

lookupVarType :: Id -> TypeCheckM (Maybe Type)
lookupVarType var =
  gets (Map.lookup var . typeEnv)

-- Generates a fresh unification variable.
freshType :: TypeCheckM Type
freshType = do
  x <- gets nextVar
  modify $ \s -> s { nextVar = nextVar s + 1 }
  pure $ TUnknown x

-- | Tries to unify 2 types, emitting an error in case it fails to unify.
unifyType :: Type -> Type -> TypeCheckM ()
unifyType t1 t2 = do
  -- TODO: add context for better error message
  subst <- gets substitution
  unifyType' (substituteType subst t1) (substituteType subst t2)
  where
    unifyType' ty1 ty2 =
      case (ty1, ty2) of
        (U32, U32) ->
          pass
        (Str, Str) ->
          pass
        (TUnknown x, TUnknown y) | x == y ->
          pass
        (TUnknown u, ty) ->
          updateSubst u ty
        (ty, TUnknown u) ->
          updateSubst u ty
        _ ->
          emitError $ UnificationFailure ty1 ty2

    -- Recursively applies a substitution to a type
    substituteType :: IntMap Type -> Type -> Type
    substituteType subst = \case
      U32 ->
        U32
      Str ->
        Str
      TUnknown unknown ->
        case IntMap.lookup unknown subst of
          Nothing ->
            TUnknown unknown
          Just (TUnknown unknown') | unknown == unknown' ->
            TUnknown unknown
          Just ty ->
            substituteType subst ty

    -- Update the current substitutions
    updateSubst u ty =
      modify $ \s ->
        s { substitution = IntMap.insert u ty (substitution s) }


-- TODO: try to merge with checkDecl?
checkDecls :: TypedefMap -> AST -> [TypeError]
checkDecls typedefMap = \case
  Module _ decls ->
    -- NOTE: By doing runM once per decl, each decl is checked with a clean state
    let state = CheckState typedefMap mempty mempty
     in concatMap (runM typedefMap . checkDecl) decls
  _ ->
    panic "Unexpected AST node in 'checkDecls'"

checkDecl :: AST -> TypeCheckM ()
checkDecl = \case
  DeclareType {} ->
    pass
  Atom nodeId name args ->
    lookupRelationType name >>= \case
      Nothing ->
        emitError $ UnknownAtom nodeId name
      Just (nodeId', types) -> do
        checkArgCount name (nodeId', types) (nodeId, args)
        zipWithM_ checkExpr args types
  Rule nodeId name args clauses ->
    lookupRelationType name >>= \case
      Nothing ->
        emitError $ UnknownAtom nodeId name
      Just (nodeId', types) -> do
        checkArgCount name (nodeId', types) (nodeId, args)
        zipWithM_ checkExpr args types
        traverse_ checkDecl clauses
  _ ->
    panic "Unexpected case in 'checkDecl'"

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
    lookupVarType var >>= \case
      Nothing ->
        -- TODO: also store in context a variable was bound here for better errors?
        bindVar var expectedTy
      Just actualTy -> do
        -- NOTE: No need to call 'unifyType', typeEnv never contains unification variables!
        when (actualTy /= expectedTy) $
          emitError $ TypeMismatch nodeId actualTy expectedTy
  Assign nodeId lhs rhs -> do
    lhsTy <- inferExpr lhs
    rhsTy <- inferExpr rhs
    -- NOTE: Because inferred types of vars can contain unification variables,
    -- we need to try and unify them.
    unifyType lhsTy rhsTy
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
    lookupVarType var >>= \case
      Nothing ->
        freshType
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
