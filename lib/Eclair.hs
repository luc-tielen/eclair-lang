{-# LANGUAGE GADTs, QuasiQuotes, TemplateHaskell #-}

module Eclair
  ( parse
  , semanticAnalysis
  , transformAST
  , compileRA
  , compileEIR
  , compileLLVM
  , compile
  , emitSimplifiedAST
  , emitRA
  , emitEIR
  , emitLLVM
  , EclairError(..)
  , handleErrors
  ) where

import qualified Data.Map as M
import Eclair.AST.Lower
import Eclair.RA.Lower
import Eclair.EIR.Lower
import Eclair.Parser
import Eclair.Pretty
import Eclair.Error
import Eclair.Id
import Eclair.ArgParser (Target(..))
import Eclair.AST.IR
import Eclair.AST.Transforms
import qualified Eclair.RA.IR as RA
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.TypeSystem as TS
import qualified Eclair.AST.Analysis as SA
import LLVM.Codegen (Module, ppllvm)
import Control.Exception
import qualified Rock
import Data.GADT.Compare.TH (deriveGEq)
import Data.Some
import Data.Maybe


type Relation = RA.Relation
type RA = RA.RA
type EIR = EIR.EIR

type Config = Maybe Target


data Query a where
  Parse :: FilePath -> Query (AST, NodeId, SpanMap)
  RunSemanticAnalysis :: FilePath -> Query SA.Result
  Typecheck :: FilePath -> Query TS.TypeInfo
  TransformAST :: FilePath -> Query (AST, StringMap)
  EmitSimplifiedAST :: FilePath -> Query ()
  CompileRA :: FilePath -> Query RA
  EmitRA :: FilePath -> Query ()
  CompileEIR :: FilePath -> Query EIR
  EmitEIR :: FilePath -> Query ()
  CompileLLVM :: FilePath -> Query Module
  EmitLLVM :: FilePath -> Query ()
  StringMapping :: FilePath -> Query (Map Text Word32)

queryFilePath :: Query a -> FilePath
queryFilePath = \case
  Parse path               -> path
  RunSemanticAnalysis path -> path
  Typecheck path           -> path
  TransformAST path        -> path
  EmitSimplifiedAST path   -> path
  CompileRA path           -> path
  EmitRA path              -> path
  CompileEIR path          -> path
  EmitEIR path             -> path
  CompileLLVM path         -> path
  EmitLLVM path            -> path
  StringMapping path       -> path

queryEnum :: Query a -> Int
queryEnum = \case
  Parse {}               -> 0
  RunSemanticAnalysis {} -> 1
  Typecheck {}           -> 2
  TransformAST {}        -> 3
  EmitSimplifiedAST {}   -> 4
  CompileRA {}           -> 5
  EmitRA {}              -> 6
  CompileEIR {}          -> 7
  EmitEIR {}             -> 8
  CompileLLVM {}         -> 9
  EmitLLVM {}            -> 10
  StringMapping {}       -> 11

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt =
    hashWithSalt salt . (queryFilePath &&& queryEnum)

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) =
    hashWithSalt salt query


rules :: Config -> Rock.Rules Query
rules config = \case
  Parse path ->
    liftIO $ either (throwIO . ParseErr path) pure =<< parseFile path
  RunSemanticAnalysis path -> do
    (ast, _, spans) <- Rock.fetch (Parse path)
    result <- liftIO $ SA.runAnalysis ast
    let errors = SA.semanticErrors result
    when (SA.hasSemanticErrors result) $ do
      liftIO $ (throwIO . SemanticErr path spans) errors
    pure result
  Typecheck path -> do
    (ast, _, spans) <- Rock.fetch (Parse path)
    liftIO . either (throwIO . TypeErr path spans) pure $ TS.typeCheck ast
  TransformAST path -> do
    -- Need to run SA and typechecking before any transformations / lowering
    -- to ensure we don't perform work on invalid programs.
    -- And thanks to rock, the results will be cached anyway.
    analysis <- Rock.fetch (RunSemanticAnalysis path)
    _ <- Rock.fetch (Typecheck path)
    (ast, nodeId, spans) <- Rock.fetch (Parse path)
    pure $ simplify nodeId (SA.semanticInfo analysis) ast
  EmitSimplifiedAST path -> do
    (ast, _) <- Rock.fetch (TransformAST path)
    liftIO $ putTextLn $ printDoc ast
  StringMapping path -> do
    (_, mapping) <- Rock.fetch (TransformAST path)
    pure mapping
  CompileRA path -> do
    ast <- fst <$> Rock.fetch (TransformAST path)
    pure $ compileToRA ast
  EmitRA path -> do
    ra <- Rock.fetch (CompileRA path)
    liftIO $ putTextLn $ printDoc ra
  CompileEIR path -> do
    stringMapping <- Rock.fetch (StringMapping path)
    ra <- Rock.fetch (CompileRA path)
    typeInfo <- Rock.fetch (Typecheck path)
    pure $ compileToEIR stringMapping typeInfo ra
  EmitEIR path -> do
    eir <- Rock.fetch (CompileEIR path)
    liftIO $ putTextLn $ printDoc eir
  CompileLLVM path -> do
    eir <- Rock.fetch (CompileEIR path)
    stringMapping <- Rock.fetch (StringMapping path)
    liftIO $ compileToLLVM config stringMapping eir
  EmitLLVM path -> do
    llvmModule <- Rock.fetch (CompileLLVM path)
    liftIO $ putTextLn $ ppllvm llvmModule

runQuery :: Config -> Query a -> IO a
runQuery config query = do
  memoVar <- newIORef mempty
  let task = Rock.fetch query
  Rock.runTask (Rock.memoise memoVar $ rules config) task

parse :: Config -> FilePath -> IO AST
parse cfg =
  map (\(ast, _, _) -> ast) . runQuery cfg . Parse

semanticAnalysis :: Config -> FilePath -> IO SA.Result
semanticAnalysis cfg =
  runQuery cfg . RunSemanticAnalysis

transformAST :: Config -> FilePath -> IO (AST, StringMap)
transformAST cfg =
  runQuery cfg . TransformAST

emitSimplifiedAST :: Config -> FilePath -> IO ()
emitSimplifiedAST cfg =
  runQuery cfg . EmitSimplifiedAST

compileRA :: Config -> FilePath -> IO RA
compileRA cfg =
  runQuery cfg . CompileRA

emitRA :: Config -> FilePath -> IO ()
emitRA cfg =
  runQuery cfg . EmitRA

compileEIR :: Config -> FilePath -> IO EIR
compileEIR cfg =
  runQuery cfg . CompileEIR

emitEIR :: Config -> FilePath -> IO ()
emitEIR cfg =
  runQuery cfg . EmitEIR

compileLLVM :: Config -> FilePath -> IO Module
compileLLVM cfg =
  runQuery cfg . CompileLLVM

compile :: Config -> FilePath -> IO Module
compile =
  compileLLVM

emitLLVM :: Config -> FilePath -> IO ()
emitLLVM cfg =
  runQuery cfg . EmitLLVM

