module Eclair.RA.Interpreter
  ( interpretRA
  ) where

-- NOTE: this is a "simple" interpreter, not meant to be performant in any way,
-- but can be used to evaluate the resulting RA data.

import Protolude hiding (handle, swap)
import Protolude.Unsafe (unsafeFromJust)
import Data.List ((!!))
import Control.Monad.Catch
import Control.Monad.Extra
import Eclair.RA.IR
import Eclair.Syntax (Id, Number)
import qualified Data.Map as M
import Data.Map (Map)

type Record = [Number]
type AliasMap = Map Id Record
type DB = Map Relation [Record]

data InterpreterState
  = InterpreterState { db :: DB, aliases :: AliasMap }

data InterpretError
  = ExitLoop
  deriving Show

instance Exception InterpretError where

type InterpreterM = StateT InterpreterState IO

interpretRA :: RA -> IO DB
interpretRA ra = runInterpreter (interpret ra) where
  interpret = \case
    Module stmts ->
      traverse_ interpret stmts
    Search r alias clauses action -> do
      records <- filterM (r `basedOn` clauses) =<< gets (lookupOrInit r . db)
      for_ records $ \record -> do
        updateAlias alias record
        interpret action
    Project r values -> do
      resolvedValues <- traverse resolveValue values
      modifyDB $ project r resolvedValues
    Merge r1 r2 ->
      modifyDB $ merge r1 r2
    Swap r1 r2 ->
      modifyDB $ swap r1 r2
    Purge r ->
      modifyDB $ purge r
    Par stmts ->
      -- TODO make parallel
      traverse_ interpret stmts
    Loop stmts ->
      handle loopExitError $
        traverse_ interpret (cycle stmts)
    Exit rs -> do
      database <- gets db
      let values = map (flip lookupOrInit database) rs
      if all null values
        then throwM ExitLoop
        else pure ()
    _ ->
      panic "Unexpected case in 'interpret'!"
  loopExitError :: InterpretError -> InterpreterM ()
  loopExitError = const $ pure ()

runInterpreter :: InterpreterM a -> IO DB
runInterpreter m =
  db <$> execStateT m (InterpreterState mempty mempty)

merge :: Relation -> Relation -> DB -> DB
merge fromR toR db =
  let values = lookupOrInit fromR db
   in M.insertWith (<>) toR values db

swap :: Relation -> Relation -> DB -> DB
swap r1 r2 db =
  let valuesR1 = lookupOrInit r1 db
      valuesR2 = lookupOrInit r2 db
   in M.insert r1 valuesR2 (M.insert r2 valuesR1 db)

purge :: Relation -> DB -> DB
purge r = M.insert r mempty

resolveValue :: RA -> InterpreterM Number
resolveValue = \case
  Lit x -> pure x
  ColumnIndex r idx -> do
    value <- currentValueInSearch r
    pure $ value !! idx
  _ -> panic "Unexpected variant in 'resolveValue'"

currentValueInSearch :: Alias -> InterpreterM Record
currentValueInSearch r =
  gets (unsafeFromJust . M.lookup r . aliases)

project :: Relation -> Record -> DB -> DB
project r values db =
  M.insertWith (<>) r [values] db

modifyDB :: (DB -> DB) -> InterpreterM ()
modifyDB f =
  modify $ \s -> s { db = f (db s) }

lookupOrInit :: Relation -> DB -> [Record]
lookupOrInit key =
  M.findWithDefault mempty key

updateAlias :: Alias -> Record -> InterpreterM ()
updateAlias a r =
  modify $ \s -> s { aliases = M.insert a r (aliases s) }

basedOn :: Relation -> [RA] -> Record -> InterpreterM Bool
basedOn r clauses record = allM toPredicate clauses
  where
    toPredicate = \case
      Constrain lhs rhs ->
        (==) <$> resolveValueInClause lhs <*> resolveValueInClause rhs
      NotElem r values -> do
        records <- gets (lookupOrInit r . db)
        values' <- traverse resolveValueInClause values
        pure $ values' `elem` records
      _ -> panic "Unexpected variant in 'toPredicate'"
    resolveValueInClause = \case
      Lit x ->
        pure x
      ColumnIndex r' idx
        | r == r' -> pure $ record !! idx
        | otherwise -> do
          value <- currentValueInSearch r
          pure $ value !! idx
      _ -> panic "Unexpected variant in 'resolveValueInClause'"

