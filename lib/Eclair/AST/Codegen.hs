{-# LANGUAGE DerivingVia #-}

module Eclair.AST.Codegen
  ( CodegenM
  , runCodegen
  , toTerm
  , project
  , search
  , loop
  , parallel
  , merge
  , swap
  , purge
  , exit
  , noElemOf
  , if'
  ) where

import Data.DList (DList)
import qualified Data.DList as DList
import qualified Eclair.RA.IR as RA
import qualified Eclair.AST.IR as AST
import Eclair.Common.Location
import Eclair.Common.Literal
import Eclair.Common.Operator
import Eclair.Common.Id


type AST = AST.AST
type RA = RA.RA
type Relation = RA.Relation
type Alias = RA.Alias
type Variable = Id

type Column = Int

newtype Row = Row { unRow :: Int }
  deriving (Eq, Ord)

data LowerState
  = LowerState
  { nextNodeId :: Word32  -- NOTE: Unrelated to NodeIDs used in AST!
  -- Constraints that can be resolved directly, but need to be emitted later.
  , directConstraints :: CodegenM RA -> CodegenM RA
  -- We keep track of which alias + column maps to which variables for later
  -- generation of constraints.
  , varMapping :: DList (Alias, Column, Variable)
  }

newtype CodegenM a
  = CodegenM (RWS Row () LowerState a)
  deriving (Functor, Applicative, Monad, MonadReader Row, MonadState LowerState)
  via RWS Row () LowerState

runCodegen :: CodegenM a -> a
runCodegen (CodegenM m) =
  -- NOTE: NodeId starts at 1, since module is manually created, and has NodeId 0
  let beginState = LowerState 1 id mempty
   in fst $ evalRWS m (Row 0) beginState

freshNodeId :: CodegenM NodeId
freshNodeId = do
  next <- gets nextNodeId
  modify $ \s -> s { nextNodeId = next + 1 }
  pure $ NodeId next

project :: Relation -> [CodegenM RA] -> CodegenM RA
project r terms = do
  nodeId <- freshNodeId
  (addDirectConstraints, mapping) <- gets (directConstraints &&& varMapping)
  let grouped =
        mapping
        & toList
        & sortOn varNameOf
        & groupBy ((==) `on` varNameOf)
      eqs = map toIndirectConstraint grouped
      addIndirectConstraints = foldl' (.) id eqs

  addDirectConstraints $ addIndirectConstraints $
    RA.Project nodeId r <$> sequence terms
  where
    varNameOf (_, _, v) = v

    resolveAliasValue (a, col, _) = do
      nodeId <- freshNodeId
      pure $ RA.ColumnIndex nodeId a col

    toIndirectConstraint bindingGroup m = case bindingGroup of
      initial :| rest -> do
        aliasValue <- resolveAliasValue initial
        aliasValues <- traverse resolveAliasValue rest
        let constraints = map (if' Equals aliasValue) aliasValues
            wrapConstraints = foldl' (.) id constraints
        wrapConstraints m

search :: Relation -> [AST] -> CodegenM RA -> CodegenM RA
search r terms inner = do
  -- Potentially reset var mapping when we reach the first search,
  -- this makes it possible to easily support multiple project statements.
  maybeResetSearchState

  nodeId <- freshNodeId
  a <- relationToAlias r
  zipWithM_ (\col t -> emitSearchTerm t a col) [0..] terms
  action <- local nextRow inner
  pure $ RA.Search nodeId r a [] action
  where
    nextRow = Row . (+1) . unRow
    maybeResetSearchState = do
      Row row <- ask
      when (row == 0) $ do
        modify $ \s -> s { varMapping = mempty }

    emitSearchTerm :: AST -> Alias -> Column -> CodegenM ()
    emitSearchTerm ast a col = do
      -- Based on what the term resolved to, we might need to create additional
      -- constraints. Literals can directly be converted to a constraint, variables
      -- are solved at the end (in the project statement).
      case ast of
        AST.Lit {} ->
          addDirectConstraint ast a col
        AST.BinOp {} ->
          addDirectConstraint ast a col
        AST.Var _ v ->
          -- We append new constraints at the end.
          -- This will cause indices to always trigger as soon as possible,
          -- which narrows down the search space and speeds up the query.
          modify $ \s -> s { varMapping = DList.snoc (varMapping s) (a, col, v) }
        _ ->
          pass

    addDirectConstraint ast a col = do
      ra <- toTerm ast
      nodeId <- freshNodeId
      let aliasValue = RA.ColumnIndex nodeId a col
          constraint = if' Equals aliasValue ra
      modify $ \s -> s { directConstraints = directConstraints s . constraint }

loop :: [CodegenM RA] -> CodegenM RA
loop ms = do
  nodeId <- freshNodeId
  RA.Loop nodeId <$> sequence ms

parallel :: [CodegenM RA] -> CodegenM RA
parallel ms = do
  nodeId <- freshNodeId
  RA.Par nodeId <$> sequence ms

merge :: Relation -> Relation -> CodegenM RA
merge from' to' = do
  nodeId <- freshNodeId
  pure $ RA.Merge nodeId from' to'

swap :: Relation -> Relation -> CodegenM RA
swap r1 r2 = do
  nodeId <- freshNodeId
  pure $ RA.Swap nodeId r1 r2

purge :: Relation -> CodegenM RA
purge r = do
  nodeId <- freshNodeId
  pure $ RA.Purge nodeId r

exit :: [Relation] -> CodegenM RA
exit rs = do
  nodeId <- freshNodeId
  pure $ RA.Exit nodeId rs

noElemOf :: Relation -> [CodegenM RA] -> CodegenM RA -> CodegenM RA
noElemOf r ts m = do
  notElemNodeId <- freshNodeId
  ifNodeId <- freshNodeId
  cond <- RA.NotElem notElemNodeId r <$> sequence ts
  RA.If ifNodeId cond <$> m

if' :: ConstraintOp -> RA -> RA -> CodegenM RA -> CodegenM RA
if' op lhs rhs body = do
  cmpNodeId <- freshNodeId
  ifNodeId <- freshNodeId
  let cond = RA.CompareOp cmpNodeId op lhs rhs
  RA.If ifNodeId cond <$> body

toTerm :: AST -> CodegenM RA
toTerm ast = do
  nodeId <- freshNodeId
  case ast of
    AST.Lit _ (LNumber lit) ->
      pure $ RA.Lit nodeId lit
    AST.Var _ v -> do
      gets (find (\(_, _, v') -> v == v') . varMapping) >>= \case
        Just (alias, col, _) -> do
          pure $ RA.ColumnIndex nodeId alias col
        Nothing ->
          panic "Found ungrounded variable in 'toTerm'!"
    AST.BinOp _ op lhs rhs ->
      RA.PrimOp nodeId op <$> toTerm lhs <*> toTerm rhs
    _ ->
      panic "Unexpected case in 'toTerm'!"

relationToAlias :: Relation -> CodegenM Alias
relationToAlias r =
  asks (appendToId r . show . unRow)
