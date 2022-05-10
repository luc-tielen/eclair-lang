{-# LANGUAGE GADTs, QuasiQuotes, TemplateHaskell #-}

module Eclair
  ( parse
  , compileRA
  , compileEIR
  , compileLLVM
  , compile
  , run
  , EclairError(..)
  , handleErrors
  ) where

import qualified Data.Map as M
import Eclair.AST.Lower
import Eclair.RA.Lower
import Eclair.EIR.Lower
import Eclair.Parser
import Eclair.Id
import Eclair.AST.IR
import qualified Eclair.RA.IR as RA
import qualified Eclair.EIR.IR as EIR
import Eclair.RA.Interpreter
import qualified Eclair.TypeSystem as TS
import LLVM.AST (Module)
import Control.Exception
import LLVM.Pretty
import qualified Rock
import Data.GADT.Compare.TH (deriveGEq)
import Data.Some
import Data.Maybe


type Relation = RA.Relation
type RA = RA.RA
type EIR = EIR.EIR

data EclairError
  = ParseErr ParseError
  | TypeErr [TS.TypeError]
  deriving (Show, Exception)


data Query a where
  Parse :: FilePath -> Query AST
  Typecheck :: FilePath -> Query TS.TypeInfo
  CompileRA :: FilePath -> Query RA
  CompileEIR :: FilePath -> Query EIR
  CompileLLVM :: FilePath -> Query Module

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt = \case
    Parse path ->
      hashWithSalt salt (path, 0 :: Int)
    Typecheck path ->
      hashWithSalt salt (path, 1 :: Int)
    CompileRA path ->
      hashWithSalt salt (path, 2 :: Int)
    CompileEIR path ->
      hashWithSalt salt (path, 3 :: Int)
    CompileLLVM path ->
      hashWithSalt salt (path, 4 :: Int)

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) =
    hashWithSalt salt query

rules :: Rock.Rules Query
rules = \case
  Parse path ->
    liftIO $ either (throwIO . ParseErr) pure =<< parseFile path
  Typecheck path -> do
    ast <- Rock.fetch (Parse path)
    liftIO . either (throwIO . TypeErr) pure $ TS.typeCheck ast
  CompileRA path -> do
    ast <- Rock.fetch (Parse path)
    pure $ compileToRA ast
  CompileEIR path -> do
    ra <- Rock.fetch (CompileRA path)
    typeInfo <- Rock.fetch (Typecheck path)
    pure $ compileToEIR typeInfo ra
  CompileLLVM path -> do
    eir <- Rock.fetch (CompileEIR path)
    liftIO $ compileToLLVM eir

runQuery :: Query a -> IO a
runQuery query = do
  memoVar <- newIORef mempty
  let task = Rock.fetch query
  Rock.runTask (Rock.memoise memoVar rules) task

parse :: FilePath -> IO AST
parse = runQuery . Parse

compileRA :: FilePath -> IO RA
compileRA =
  runQuery . CompileRA

compileEIR :: FilePath -> IO EIR
compileEIR =
  runQuery . CompileEIR

compileLLVM :: FilePath -> IO Module
compileLLVM =
  runQuery . CompileLLVM

compile :: FilePath -> IO Module
compile = compileLLVM

run :: FilePath -> IO (M.Map Relation [[Number]])
run =
  interpretRA <=< runQuery . CompileRA

-- TODO: improve error handling...
handleErrors :: EclairError -> IO ()
handleErrors = \case
  ParseErr err -> do
    printParseError err
    panic "Failed to parse file."
  TypeErr errs -> do
    traverse_ print errs
    panic "Failed to type-check file."

