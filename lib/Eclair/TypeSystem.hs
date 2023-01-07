module Eclair.TypeSystem
  ( Type(..)
  , TypeError(..)
  , Context(..)
  , getContextLocation
  , TypeInfo(..)
  , DefinitionType(..)
  , TypedefInfo
  , ExternDefInfo
  , typeCheck
  ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.DList as DList
import Data.DList (DList)
import qualified Data.DList.DNonEmpty as DNonEmpty
import Data.DList.DNonEmpty (DNonEmpty)
import Control.Monad.Extra
import Eclair.AST.IR
import Eclair.Common.Id
import Eclair.Common.Location (NodeId)

-- NOTE: This module contains a lot of partial functions due to the fact
-- that the rest of the compiler relies heavily on recursion-schemes.
-- This is however one place where I have not figured out how to apply a
-- recursion-scheme here.. yet!
--
-- TODO: Maybe a variant of a mutumorphism might work here?

data DefinitionType
  = ConstraintType [Type]
  | FunctionType [Type] Type
  deriving (Eq, Show)

type ExternDefInfo = Map Id DefinitionType

type TypedefInfo = Map Id [Type]

data TypeInfo
  = TypeInfo
  { infoTypedefs :: TypedefInfo  -- NOTE: only typedefs are needed, no external definitions
  , resolvedTypes :: Map NodeId Type
  } deriving Show

instance Semigroup TypeInfo where
  TypeInfo tdInfo1 resolved1 <> TypeInfo tdInfo2 resolved2 =
    TypeInfo (tdInfo1 <> tdInfo2)  (resolved1 <> resolved2)

instance Monoid TypeInfo where
  mempty =
    TypeInfo mempty mempty

data Context loc
  = WhileChecking loc
  | WhileInferring loc
  | WhileUnifying loc
  deriving (Eq, Ord, Show, Functor)

-- NOTE: for now, no actual types are checked since everything is a u32.
data TypeError loc
  = UnknownAtom loc Id
  | ArgCountMismatch Id (loc, Int) (loc, Int)
  | TypeMismatch loc Type Type (NonEmpty (Context loc))  -- 1st type is actual, 2nd is expected
  | UnificationFailure Type Type (NonEmpty (Context loc))
  | HoleFound loc (NonEmpty (Context loc)) Type (Map Id Type)
  -- 1st is error location, 2nd is definition location.
  | UnexpectedFunctionType loc loc
  | UnexpectedConstraintType loc loc
  deriving (Eq, Ord, Show, Functor)

-- Only used internally in this module.
type Ctx = Context NodeId
type TypeErr = TypeError NodeId

type DefMap = Map Id (NodeId, DefinitionType)
type UnresolvedHole = Type -> Map Id Type -> TypeErr

data Env
  = Env
  { defs :: DefMap
  , tcContext :: DNonEmpty Ctx
  }

-- State used to report type information back to the user (via LSP)
data TrackingState
  = TrackingState
  { directlyResolvedTypes :: Map NodeId Type  -- literals are immediately resolved
  , trackedVariables :: [(Id, NodeId)]
  }

data CheckState
  = CheckState
  { typeEnv :: Map Id Type
  , substitution :: IntMap Type
  , varCounter :: Int
  , errors :: DList TypeErr
  , unresolvedHoles :: DList (Type, UnresolvedHole)
  -- The tracking state is not needed for the typechecking algorithm,
  -- but is used to report information back to the user (via LSP).
  , trackingState :: TrackingState
  }

newtype TypeCheckM a =
  TypeCheckM (RWS Env () CheckState a)
  deriving (Functor, Applicative, Monad, MonadState CheckState, MonadReader Env)
  via (RWS Env () CheckState)


typeCheck :: AST -> Either [TypeErr] TypeInfo
typeCheck ast
  | null typeErrors = pure $ TypeInfo (map snd typeDefMap) resolvedTys
  | otherwise = throwError typeErrors
  where
    (typeDefMap, externDefMap) = cata (combine extractTypedefs extractExternDefs) ast
    defMap = map (map ConstraintType) typeDefMap <> externDefMap
    (typeErrors, resolvedTys) = checkDecls defMap ast

    combine f g = f . map fst &&& g . map snd

    extractTypedefs = \case
      DeclareTypeF nodeId name tys _ ->
        one (name, (nodeId, tys))
      AtomF {} -> mempty
      RuleF {} -> mempty
      astf -> fold astf

    extractExternDefs = \case
      ExternDefinitionF nodeId name tys mRetTy ->
        one (name, (nodeId, maybe (ConstraintType tys) (FunctionType tys) mRetTy))
      AtomF {} -> mempty
      RuleF {} -> mempty
      astf -> fold astf

-- TODO: try to merge with checkDecl?
checkDecls :: DefMap -> AST -> ([TypeErr], Map NodeId Type)
checkDecls defMap = \case
  Module _ decls ->
    -- NOTE: By using runM once per decl, each decl is checked with a clean state
    let results = map (\d -> runM (beginContext d) defMap $ checkDecl d) decls
     in bimap fold fold $ partitionEithers results
  _ ->
    panic "Unexpected AST node in 'checkDecls'"
  where
    beginContext d = WhileChecking (getNodeId d)

checkDecl :: AST -> TypeCheckM ()
checkDecl ast = case ast of
  DeclareType {} ->
    pass
  ExternDefinition {} ->
    pass
  Atom nodeId name args -> do
    ctx <- getContext
    -- TODO find better way to update context only for non-top level atoms
    let updateCtx = if isTopLevel ctx then id else addCtx
    updateCtx $ lookupRelationType name >>= \case
      Nothing ->
        emitError $ UnknownAtom nodeId name
      Just (nodeId', ConstraintType types) -> do
        checkArgCount name (nodeId', types) (nodeId, args)
        zipWithM_ checkExpr args types
      Just (nodeId', FunctionType {}) -> do
        emitError $ UnexpectedFunctionType nodeId nodeId'

    processUnresolvedHoles

  Rule nodeId name args clauses -> do
    lookupRelationType name >>= \case
      Nothing ->
        emitError $ UnknownAtom nodeId name
      Just (nodeId', ConstraintType types) -> do
        checkArgCount name (nodeId', types) (nodeId, args)
        zipWithM_ checkExpr args types
      Just (nodeId', FunctionType {}) -> do
        emitError $ UnexpectedFunctionType nodeId nodeId'

    traverse_ checkDecl clauses
    processUnresolvedHoles

  Constraint nodeId op lhs rhs -> addCtx $ do
    if isEqualityOp op
      then do
        lhsTy <- inferExpr lhs
        rhsTy <- inferExpr rhs
        -- NOTE: Because inferred types of vars can contain unification variables,
        -- we need to try and unify them.
        unifyType nodeId lhsTy rhsTy
      else do
        -- Comparison => both sides need to be numbers
        checkExpr lhs U32
        checkExpr rhs U32
  _ ->
    panic "Unexpected case in 'checkDecl'"
  where
    addCtx = addContext (WhileChecking $ getNodeId ast)
    isTopLevel = (==1) . length

checkArgCount :: Id -> (NodeId, [Type]) -> (NodeId, [AST]) -> TypeCheckM ()
checkArgCount name (nodeIdTypedef, types) (nodeId, args) = do
  let actualArgCount = length args
      expectedArgCount = length types
  when (actualArgCount /= expectedArgCount) $
    emitError $ ArgCountMismatch name (nodeIdTypedef, expectedArgCount) (nodeId, actualArgCount)

checkExpr :: AST -> Type -> TypeCheckM ()
checkExpr ast expectedTy = do
  let nodeId = getNodeId ast
  addContext (WhileChecking nodeId) $ case ast of
    l@Lit {} -> do
      actualTy <- inferExpr l
      -- NOTE: No need to call 'unifyType', types of literals are always concrete types.
      when (actualTy /= expectedTy) $ do
        ctx <- getContext
        emitError $ TypeMismatch nodeId actualTy expectedTy ctx
    PWildcard {} ->
      -- NOTE: no checking happens for wildcards, since they always refer to
      -- unique vars (and thus cannot cause type errors)
      trackDirectlyResolvedType nodeId expectedTy
    Var _ var -> do
      trackVariable nodeId var

      lookupVarType var >>= \case
        Nothing ->
          -- TODO: also store in context/state a variable was bound here for better errors?
          bindVar var expectedTy
        Just actualTy -> do
          when (actualTy /= expectedTy) $ do
            ctx <- getContext
            emitError $ TypeMismatch nodeId actualTy expectedTy ctx
    Hole {} -> do
      holeTy <- emitHoleFoundError nodeId
      unifyType nodeId holeTy expectedTy
    BinOp _ _ lhs rhs -> do
      -- Arithmetic expressions always need to be numbers.
      checkExpr lhs U32
      checkExpr rhs U32
    Atom _ name args -> do
      lookupRelationType name >>= \case
        Just (nodeId', FunctionType types actualRetTy) -> do
          checkArgCount name (nodeId', types) (nodeId, args)
          zipWithM_ checkExpr args types
          when (actualRetTy /= expectedTy) $ do
            ctx <- getContext
            emitError $ TypeMismatch nodeId actualRetTy expectedTy ctx
        Just (nodeId', ConstraintType {}) -> do
          emitError $ UnexpectedConstraintType nodeId nodeId'
        Nothing ->
          emitError $ UnknownAtom nodeId name
    e -> do
      -- Basically an unexpected / unhandled case => try inferring as a last resort.
      actualTy <- inferExpr e
      unifyType (getNodeId e) actualTy expectedTy

inferExpr :: AST -> TypeCheckM Type
inferExpr ast = do
  let nodeId = getNodeId ast
  addContext (WhileInferring nodeId) $ case ast of
    Lit _ lit -> do
      let ty = case lit of
            LNumber {} -> U32
            LString {} -> Str
      trackDirectlyResolvedType nodeId ty
      pure ty
    Var _ var -> do
      trackVariable nodeId var

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
    Hole {} ->
      emitHoleFoundError nodeId
    BinOp _ _ lhs rhs -> do
      -- Arithmetic expressions always need to be numbers.
      checkExpr lhs U32
      checkExpr rhs U32
      pure U32
    Atom _ name args -> do
      lookupRelationType name >>= \case
        Just (nodeId', FunctionType types retTy) -> do
          checkArgCount name (nodeId', types) (nodeId, args)
          zipWithM_ checkExpr args types
          pure retTy
        Just (nodeId', ConstraintType {}) -> do
          emitError $ UnexpectedConstraintType nodeId nodeId'
          -- We generate a fresh type which will always unify, but typechecking will fail anyway
          freshType
        Nothing -> do
          emitError $ UnknownAtom nodeId name
          -- We generate a fresh type which will always unify, but typechecking will fail anyway
          freshType
    _ ->
      panic "Unexpected case in 'inferExpr'"

runM :: Ctx -> DefMap -> TypeCheckM a -> Either [TypeErr] (Map NodeId Type)
runM ctx defMap action =
  let (TypeCheckM m) = action *> computeTypeInfoById
      trackState = TrackingState mempty mempty
      tcState = CheckState mempty mempty 0 mempty mempty trackState
      env = Env defMap (pure ctx)
      (typeInfoById, endState, _) = runRWS m env tcState
      errs = toList . errors $ endState
   in if null errs then Right typeInfoById else Left errs

emitError :: TypeErr -> TypeCheckM ()
emitError err =
  modify $ \s -> s { errors = DList.snoc (errors s) err }

bindVar :: Id -> Type -> TypeCheckM ()
bindVar var ty =
  modify $ \s -> s { typeEnv = Map.insert var ty (typeEnv s) }

lookupRelationType :: Id -> TypeCheckM (Maybe (NodeId, DefinitionType))
lookupRelationType name =
  asks (Map.lookup name . defs)

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
  x <- gets varCounter
  modify $ \s -> s { varCounter = varCounter s + 1 }
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

computeTypeInfoById :: TypeCheckM (Map NodeId Type)
computeTypeInfoById = do
  ts <- gets trackingState
  let vars = Map.toList $ Map.fromListWith (<>) $ one <<$>> trackedVariables ts
  resolvedVarTypes <- flip concatMapM vars $ \(var, nodeIds) -> do
    varTy <- lookupVarType var
    pure $ mapMaybe (\nodeId -> (nodeId,) <$> varTy) nodeIds

  pure $ directlyResolvedTypes ts <> Map.fromList resolvedVarTypes

addContext :: Ctx -> (TypeCheckM a -> TypeCheckM a)
addContext ctx = local $ \env ->
  env { tcContext = tcContext env `DNonEmpty.snoc` ctx }

getContext :: TypeCheckM (NonEmpty Ctx)
getContext =
  asks (DNonEmpty.toNonEmpty . tcContext)

getContextLocation :: Context loc -> loc
getContextLocation = \case
  WhileChecking loc -> loc
  WhileInferring loc -> loc
  WhileUnifying loc -> loc

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

trackDirectlyResolvedType :: NodeId -> Type -> TypeCheckM ()
trackDirectlyResolvedType nodeId ty = do
  ts <- gets trackingState
  let ts' = ts { directlyResolvedTypes = directlyResolvedTypes ts <> one (nodeId, ty) }
  modify $ \s -> s { trackingState = ts' }

trackVariable :: NodeId -> Id -> TypeCheckM ()
trackVariable nodeId var = do
  ts <- gets trackingState
  let ts' = ts { trackedVariables = (var, nodeId) : trackedVariables ts }
  modify $ \s -> s { trackingState = ts' }

