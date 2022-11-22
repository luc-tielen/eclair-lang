module Eclair.TypeSystem
  ( Type(..)
  , TypeError(..)
  , Context(..)
  , TypeInfo
  , typeCheck
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Eclair.AST.IR
import Eclair.Id
import qualified Data.DList as DList
import Data.DList (DList)
import qualified Data.DList.DNonEmpty as DNonEmpty
import Data.DList.DNonEmpty (DNonEmpty)

-- NOTE: This module contains a lot of partial functions due to the fact
-- that the rest of the compiler relies heavily on recursion-schemes.
-- This is however one place where I have not figured out how to apply a
-- recursion-scheme here.. yet!


type TypeInfo = Map Id [Type]

data Context
  = WhileChecking NodeId
  | WhileInferring NodeId
  | WhileUnifying NodeId
  deriving (Eq, Ord, Show)

-- NOTE: for now, no actual types are checked since everything is a u32.
data TypeError
  = UnknownAtom NodeId Id
  | ArgCountMismatch Id (NodeId, Int) (NodeId, Int)
  | DuplicateTypeDeclaration Id (NonEmpty (NodeId, [Type]))
  | TypeMismatch NodeId Type Type (NonEmpty Context)  -- 1st type is actual, 2nd is expected
  | UnificationFailure Type Type (NonEmpty Context)
  | HoleFound NodeId (NonEmpty Context) Type (Map Id Type)
  deriving (Eq, Ord, Show)

typeCheck :: AST -> Either [TypeError] TypeInfo
typeCheck ast
  | null errors' = pure $ map snd typedefMap
  | otherwise   = throwError errors'
  where
    errors' = checkDecls typedefMap ast ++ duplicateErrors typedefs'
    typedefs' = extractTypeDefs ast
    typedefMap = Map.fromList typedefs'

type TypedefMap = Map Id (NodeId, [Type])
type UnresolvedHole = Type -> Map Id Type -> TypeError

data CheckState
  = CheckState
  { typedefs :: TypedefMap
  , typeEnv :: Map Id Type
  , substitution :: IntMap Type
  , nextVar :: Int
  , errors :: DList TypeError
  , unresolvedHoles :: DList (Type, UnresolvedHole)
  }

type TypeCheckM = ReaderT (DNonEmpty Context) (State CheckState)

runM :: Context -> TypedefMap -> TypeCheckM a -> [TypeError]
runM ctx typedefMap m =
  let tcState = CheckState typedefMap mempty mempty 0 mempty mempty
   in toList . errors $ execState (runReaderT m (pure ctx)) tcState

addContext :: Context -> (TypeCheckM a -> TypeCheckM a)
addContext ctx = local (`DNonEmpty.snoc` ctx)

getContext :: TypeCheckM (NonEmpty Context)
getContext = asks DNonEmpty.toNonEmpty

emitError :: TypeError -> TypeCheckM ()
emitError err =
  modify $ \s -> s { errors = DList.snoc (errors s) err }

bindVar :: Id -> Type -> TypeCheckM ()
bindVar var ty =
  modify $ \s -> s { typeEnv = Map.insert var ty (typeEnv s) }

lookupRelationType :: Id -> TypeCheckM (Maybe (NodeId, [Type]))
lookupRelationType name =
  gets (Map.lookup name . typedefs)

-- A variable can either be a concrete type, or a unification variable.
-- In case of unification variable, try looking it up in current substitution.
-- As a result, this will make it so variables in a rule body with same name have the same type.
-- NOTE: if this ends up not being powerful enough, need to upgrade to SCC + a solver for each group of "linked" variables.
lookupVarType :: Id -> TypeCheckM (Maybe Type)
lookupVarType var = do
  (maybeVarTy, subst) <- gets (Map.lookup var . typeEnv &&& substitution)
  case maybeVarTy of
    Just (TUnknown u) -> pure $ IntMap.lookup u subst
    maybeTy -> pure maybeTy

-- Generates a fresh unification variable.
freshType :: TypeCheckM Type
freshType = do
  x <- gets nextVar
  modify $ \s -> s { nextVar = nextVar s + 1 }
  pure $ TUnknown x

-- | Tries to unify 2 types, emitting an error in case it fails to unify.
unifyType :: NodeId -> Type -> Type -> TypeCheckM ()
unifyType nodeId t1 t2 = addContext (WhileUnifying nodeId) $ do
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
        _ -> do
          ctx <- getContext
          emitError $ UnificationFailure ty1 ty2 ctx

    -- Update the current substitutions
    updateSubst :: Int -> Type -> TypeCheckM ()
    updateSubst u ty =
      modify $ \s ->
        s { substitution = IntMap.insert u ty (substitution s) }

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


-- TODO: try to merge with checkDecl?
checkDecls :: TypedefMap -> AST -> [TypeError]
checkDecls typedefMap = \case
  Module _ decls ->
    -- NOTE: By doing runM once per decl, each decl is checked with a clean state
    let beginContext d =
          WhileChecking (getNodeId d)
     in concatMap (\d -> runM (beginContext d) typedefMap $ checkDecl d) decls
  _ ->
    panic "Unexpected AST node in 'checkDecls'"

checkDecl :: AST -> TypeCheckM ()
checkDecl ast = case ast of
  DeclareType {} ->
    pass
  Atom nodeId name args -> do
    ctx <- getContext
    -- TODO find better way to update context only for non-top level atoms
    let updateCtx = if isTopLevel ctx then id else addCtx
    updateCtx $ lookupRelationType name >>= \case
      Nothing ->
        emitError $ UnknownAtom nodeId name
      Just (nodeId', types) -> do
        checkArgCount name (nodeId', types) (nodeId, args)
        zipWithM_ checkExpr args types

    processUnresolvedHoles

  Rule nodeId name args clauses -> do
    lookupRelationType name >>= \case
      Nothing ->
        emitError $ UnknownAtom nodeId name
      Just (nodeId', types) -> do
        checkArgCount name (nodeId', types) (nodeId, args)
        zipWithM_ checkExpr args types

    traverse_ checkDecl clauses
    processUnresolvedHoles

  Assign nodeId lhs rhs -> addCtx $ do
    lhsTy <- inferExpr lhs
    rhsTy <- inferExpr rhs
    -- NOTE: Because inferred types of vars can contain unification variables,
    -- we need to try and unify them.
    unifyType nodeId lhsTy rhsTy
  _ ->
    panic "Unexpected case in 'checkDecl'"
  where
    addCtx = addContext (WhileChecking $ getNodeId ast)
    isTopLevel = (==1) . length

checkArgCount :: Id -> (NodeId, [Type]) -> (NodeId, [AST]) -> TypeCheckM ()
checkArgCount name (nodeIdTypeDef, types) (nodeId, args) = do
  let actualArgCount = length args
      expectedArgCount = length types
  when (actualArgCount /= expectedArgCount) $
    emitError $ ArgCountMismatch name (nodeIdTypeDef, expectedArgCount) (nodeId, actualArgCount)

checkExpr :: AST -> Type -> TypeCheckM ()
checkExpr ast expectedTy = addContext (WhileChecking $ getNodeId ast) $ case ast of
  l@(Lit nodeId _) -> do
    actualTy <- inferExpr l
    -- NOTE: No need to call 'unifyType', types of literals are always concrete types.
    when (actualTy /= expectedTy) $ do
      ctx <- getContext
      emitError $ TypeMismatch nodeId actualTy expectedTy ctx
  PWildcard {} ->
    -- NOTE: no checking happens for wildcards, since they always refer to
    -- unique vars (and thus cannot cause type errors)
    pass
  Var nodeId var -> do
    lookupVarType var >>= \case
      Nothing ->
        -- TODO: also store in context/state a variable was bound here for better errors?
        bindVar var expectedTy
      Just actualTy -> do
        when (actualTy /= expectedTy) $ do
          ctx <- getContext
          emitError $ TypeMismatch nodeId actualTy expectedTy ctx
  Hole nodeId -> do
    holeTy <- emitHoleFoundError nodeId
    unifyType nodeId holeTy expectedTy
  e -> do
    -- Basically an unexpected / unhandled case => try inferring as a last resort.
    actualTy <- inferExpr e
    unifyType (getNodeId e) actualTy expectedTy

inferExpr :: AST -> TypeCheckM Type
inferExpr ast = addContext (WhileInferring $ getNodeId ast) $ case ast of
  Lit _ lit ->
    case lit of
      LNumber {} ->
        pure U32
      LString {} ->
        pure Str
  Var _ var -> do
    lookupVarType var >>= \case
      Nothing -> do
        ty <- freshType
        -- This introduces potentially a unification variable into the type environment, but we need this
        -- for complicated rule bodies where a variable occurs in more than one place.
        -- (Otherwise a variable is treated as new each time).
        bindVar var ty
        pure ty
      Just ty ->
        pure ty
  Hole nodeId ->
    emitHoleFoundError nodeId
  _ ->
    panic "Unexpected case in 'inferExpr'"

emitHoleFoundError :: NodeId -> TypeCheckM Type
emitHoleFoundError nodeId = do
  ty <- freshType   -- We generate a fresh type, and use this later to figure out the type of the hole.
  ctx <- getContext
  modify' $ \s ->
    s { unresolvedHoles = DList.snoc (unresolvedHoles s) (ty, HoleFound nodeId ctx) }
  pure ty

processUnresolvedHoles :: TypeCheckM ()
processUnresolvedHoles = do
  holes <- gets (toList . unresolvedHoles)
  unless (null holes) $ do
    (env, subst) <- gets (typeEnv &&& substitution)
    let solvedEnv = map (substituteType subst) env
    forM_ holes $ \(holeTy, hole) ->
      emitError $ hole (substituteType subst holeTy) solvedEnv

    modify' $ \s -> s { unresolvedHoles = mempty }

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

getNodeId :: AST -> NodeId
getNodeId = \case
  Module nodeId _ -> nodeId
  DeclareType nodeId _ _ -> nodeId
  Rule nodeId _ _ _ -> nodeId
  Atom nodeId _ _ -> nodeId
  Assign nodeId _ _ -> nodeId
  Lit nodeId _ -> nodeId
  Var nodeId _ -> nodeId
  Hole nodeId -> nodeId
