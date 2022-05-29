{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Eclair
  ( parse,
    compileRA,
    compileEIR,
    compileLLVM,
    compile,
    emitRA,
    emitEIR,
    emitLLVM,
    run,
    EclairError (..),
    handleErrors,
  )
where

import Control.Exception
import Data.GADT.Compare
import Data.GADT.Compare.TH (deriveGEq, geq', runGComparing)
import qualified Data.Map as M
import Data.Maybe
import Data.Some
import Eclair.AST.IR
import Eclair.AST.Lower
import qualified Eclair.EIR.IR as EIR
import Eclair.EIR.Lower
import Eclair.Id
import Eclair.Parser
import Eclair.Pretty
import qualified Eclair.RA.IR as RA
import Eclair.RA.Interpreter
import Eclair.RA.Lower
import qualified Eclair.TypeSystem as TS
import LLVM.Codegen (Module, ppllvm)
import qualified Rock

type Relation = RA.Relation

type RA = RA.RA

type EIR = EIR.EIR

data EclairError
  = ParseErr ParseError
  | TypeErr [TS.TypeError]
  deriving (Show, Exception)

data Query a where
  Parse :: FilePath -> Query (AST, SpanMap)
  Typecheck :: FilePath -> Query TS.TypeInfo
  CompileRA :: FilePath -> Query RA
  EmitRA :: FilePath -> Query ()
  CompileEIR :: FilePath -> Query EIR
  EmitEIR :: FilePath -> Query ()
  CompileLLVM :: FilePath -> Query Module
  EmitLLVM :: FilePath -> Query ()

queryFilePath :: Query a -> FilePath
queryFilePath = \case
  Parse path -> path
  Typecheck path -> path
  CompileRA path -> path
  EmitRA path -> path
  CompileEIR path -> path
  EmitEIR path -> path
  CompileLLVM path -> path
  EmitLLVM path -> path

queryEnum :: Query a -> Int
queryEnum = \case
  Parse {} -> 0
  Typecheck {} -> 1
  CompileRA {} -> 2
  EmitRA {} -> 3
  CompileEIR {} -> 4
  EmitEIR {} -> 5
  CompileLLVM {} -> 6
  EmitLLVM {} -> 7

deriveGEq ''Query

instance Eq (Query a) where
  Parse x == Parse y = x == y
  Typecheck x == Typecheck y = x == y
  CompileRA x == CompileRA y = x == y
  EmitRA x == EmitRA y = x == y
  CompileEIR x == CompileEIR y = x == y
  EmitEIR x == EmitEIR y = x == y
  CompileLLVM x == CompileLLVM y = x == y
  EmitLLVM x == EmitLLVM y = x == y

instance Hashable (Query a) where
  hashWithSalt salt =
    hashWithSalt salt . (queryFilePath &&& queryEnum)

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) =
    hashWithSalt salt query

rules :: Rock.Rules Query
rules = \case
  Parse path ->
    liftIO $ either (throwIO . ParseErr) pure =<< parseFile path
  Typecheck path -> do
    ast <- fst <$> Rock.fetch (Parse path)
    liftIO . either (throwIO . TypeErr) pure $ TS.typeCheck ast
  CompileRA path -> do
    ast <- fst <$> Rock.fetch (Parse path)
    pure $ compileToRA ast
  EmitRA path -> do
    ra <- Rock.fetch (CompileRA path)
    liftIO $ putTextLn $ printDoc ra
  CompileEIR path -> do
    ra <- Rock.fetch (CompileRA path)
    typeInfo <- Rock.fetch (Typecheck path)
    pure $ compileToEIR typeInfo ra
  EmitEIR path -> do
    eir <- Rock.fetch (CompileEIR path)
    liftIO $ putTextLn $ printDoc eir
  CompileLLVM path -> do
    eir <- Rock.fetch (CompileEIR path)
    liftIO $ compileToLLVM eir
  EmitLLVM path -> do
    llvmModule <- Rock.fetch (CompileLLVM path)
    liftIO $ putTextLn $ ppllvm llvmModule

runQuery :: Query a -> IO a
runQuery query = do
  memoVar <- newIORef mempty
  let task = Rock.fetch query
  Rock.runTask (Rock.memoise memoVar rules) task

parse :: FilePath -> IO AST
parse = map fst . runQuery . Parse

compileRA :: FilePath -> IO RA
compileRA =
  runQuery . CompileRA

emitRA :: FilePath -> IO ()
emitRA =
  runQuery . EmitRA

compileEIR :: FilePath -> IO EIR
compileEIR =
  runQuery . CompileEIR

emitEIR :: FilePath -> IO ()
emitEIR =
  runQuery . EmitEIR

compileLLVM :: FilePath -> IO Module
compileLLVM =
  runQuery . CompileLLVM

compile :: FilePath -> IO Module
compile = compileLLVM

emitLLVM :: FilePath -> IO ()
emitLLVM = runQuery . EmitLLVM

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
