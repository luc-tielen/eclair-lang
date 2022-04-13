module Eclair.RA.Interpreter
  ( interpretRA
  ) where

-- NOTE: this is a "simple" interpreter, not meant to be performant in any way,
-- but can be used to evaluate the resulting RA data.

import Protolude hiding (handle, swap)
import Data.Maybe (fromJust)
import Data.IORef
import Data.List ((!!))
import Control.Monad.Catch
import Control.Monad.Extra
import Eclair.RA.IR
import Eclair.AST.IR (Number)
import Eclair.Id
import qualified Data.Map as M

type Record = [Number]
type AliasMap = Map Id Record
type DB = Map Relation [Record]

data InterpreterState
  = InterpreterState { db :: DB, aliases :: AliasMap }

data InterpretError
  = ExitLoop
  deriving Show

instance Exception InterpretError where

type InterpreterM = ReaderT (IORef InterpreterState) IO

interpretRA :: RA -> IO DB
interpretRA ra = removeInternals <$> runInterpreter (interpret ra) where
  interpret = \case
    Module stmts ->
      traverse_ interpret stmts
    Search r alias clauses action -> do
      records <- filterM (alias `basedOn` clauses) . lookupOrInit r . db =<< readRef
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
      database <- db <$> readRef
      let values = map (`lookupOrInit` database) rs
      if all null values
        then throwM ExitLoop
        else pure ()
    _ ->
      panic "Unexpected case in 'interpret'!"
  loopExitError :: InterpretError -> InterpreterM ()
  loopExitError = const $ pure ()
  removeInternals =
    M.mapMaybeWithKey (\k v -> if isInternalKey k then Nothing else Just v)
  isInternalKey k =
    k `startsWithId` Id "delta_" ||
    k `startsWithId` Id "new_"

runInterpreter :: InterpreterM a -> IO DB
runInterpreter m = do
  stateRef <- newIORef (InterpreterState mempty mempty)
  void $ runReaderT m stateRef
  db <$> readIORef stateRef

merge :: Relation -> Relation -> DB -> DB
merge fromR toR kb =
  let values = lookupOrInit fromR kb
   in M.insertWith (<>) toR values kb

swap :: Relation -> Relation -> DB -> DB
swap r1 r2 kb =
  let valuesR1 = lookupOrInit r1 kb
      valuesR2 = lookupOrInit r2 kb
   in M.insert r1 valuesR2 (M.insert r2 valuesR1 kb)

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
  fromJust . M.lookup r . aliases <$> readRef

project :: Relation -> Record -> DB -> DB
project r values =
  M.insertWith (<>) r [values]

modifyDB :: (DB -> DB) -> InterpreterM ()
modifyDB f =
  modifyRef $ \s -> s { db = f (db s) }

lookupOrInit :: Relation -> DB -> [Record]
lookupOrInit = M.findWithDefault mempty

updateAlias :: Alias -> Record -> InterpreterM ()
updateAlias a r =
  modifyRef $ \s -> s { aliases = M.insert a r (aliases s) }

basedOn :: Alias -> [RA] -> Record -> InterpreterM Bool
basedOn a clauses record = allM toPredicate clauses
  where
    toPredicate = \case
      Constrain lhs rhs ->
        (==) <$> resolveValueInClause lhs <*> resolveValueInClause rhs
      NotElem r values -> do
        records <- lookupOrInit r . db <$> readRef
        values' <- traverse resolveValueInClause values
        pure $ values' `notElem` records
      _ -> panic "Unexpected variant in 'toPredicate'"
    resolveValueInClause = \case
      Lit x ->
        pure x
      ColumnIndex a' idx
        | a == a' -> pure $ record !! idx
        | otherwise -> do
          value <- currentValueInSearch a'
          pure $ value !! idx
      _ -> panic "Unexpected variant in 'resolveValueInClause'"

readRef :: InterpreterM InterpreterState
readRef = do
  ref <- ask
  liftIO $ readIORef ref

modifyRef :: (InterpreterState -> InterpreterState) -> InterpreterM ()
modifyRef f = do
  ref <- ask
  liftIO $ modifyIORef ref f

