{-# LANGUAGE GADTs, QuasiQuotes, TemplateHaskell, PackageImports, StandaloneDeriving #-}

module Eclair
  ( parse
  , semanticAnalysis
  , typeCheck
  , emitDiagnostics
  , emitSimplifiedAST
  , emitRA
  , emitEIR
  , emitLLVM
  , Parameters(..)
  , EclairError(..)
  , handleErrorsCLI
  ) where

import Control.Exception
import Eclair.AST.Lower
import Eclair.RA.Lower
import Eclair.EIR.Lower
import Eclair.Parser
import Eclair.Common.Pretty
import Eclair.Error
import Eclair.Common.Id
import Eclair.Common.Config (Target(..))
import Eclair.AST.IR
import Eclair.AST.Transforms
import qualified Eclair.RA.IR as RA
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.TypeSystem as TS
import qualified Eclair.AST.Analysis as SA
import LLVM.Codegen (Module, ppllvm)
import qualified Rock
import Data.GADT.Compare.TH (deriveGEq)
import "dependent-sum" Data.Some


type RA = RA.RA
type EIR = EIR.EIR

type Config = Maybe Target


data Query a where
  Parse :: FilePath -> Query (AST, NodeId, SpanMap)
  RunSemanticAnalysis :: FilePath -> Query SA.SemanticInfo
  Typecheck :: FilePath -> Query TS.TypeInfo
  EmitDiagnostics :: FilePath -> Query ()
  TransformAST :: FilePath -> Query (AST, StringMap)
  EmitSimplifiedAST :: FilePath -> Query ()
  CompileRA :: FilePath -> Query RA
  EmitRA :: FilePath -> Query ()
  CompileEIR :: FilePath -> Query EIR
  EmitEIR :: FilePath -> Query ()
  CompileLLVM :: FilePath -> Query Module
  EmitLLVM :: FilePath -> Query ()
  StringMapping :: FilePath -> Query (Map Text Word32)
  UsageMapping :: FilePath -> Query (Map Id UsageMode)

deriving instance Eq (Query a)

queryFilePath :: Query a -> FilePath
queryFilePath = \case
  Parse path               -> path
  RunSemanticAnalysis path -> path
  Typecheck path           -> path
  EmitDiagnostics path     -> path
  TransformAST path        -> path
  EmitSimplifiedAST path   -> path
  CompileRA path           -> path
  EmitRA path              -> path
  CompileEIR path          -> path
  EmitEIR path             -> path
  CompileLLVM path         -> path
  EmitLLVM path            -> path
  StringMapping path       -> path
  UsageMapping path        -> path

queryEnum :: Query a -> Int
queryEnum = \case
  Parse {}               -> 0
  RunSemanticAnalysis {} -> 1
  Typecheck {}           -> 2
  EmitDiagnostics {}     -> 3
  TransformAST {}        -> 4
  EmitSimplifiedAST {}   -> 5
  CompileRA {}           -> 6
  EmitRA {}              -> 7
  CompileEIR {}          -> 8
  EmitEIR {}             -> 9
  CompileLLVM {}         -> 10
  EmitLLVM {}            -> 11
  StringMapping {}       -> 12
  UsageMapping {}        -> 13

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt =
    hashWithSalt salt . (queryFilePath &&& queryEnum)

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) =
    hashWithSalt salt query

data Parameters
  = Parameters
  { paramsConfig :: !Config
  , paramsReadSourceFile :: FilePath -> IO (Maybe Text)
  }

rules :: Rock.Task Query ()
      -> Parameters
      -> Rock.GenRules (Rock.Writer [EclairError] Query) Query
rules abortOnError params (Rock.Writer query) = case query of
  Parse path -> liftIO $ do
    (ast, nodeId, spanMap, mParseErr) <- parseFile (paramsReadSourceFile params) path
    pure ((ast, nodeId, spanMap), ParseErr path <$> maybeToList mParseErr)
  RunSemanticAnalysis path -> do
    (ast, _, spans) <- Rock.fetch (Parse path)
    result <- liftIO $ SA.runAnalysis ast
    let errs = if SA.hasSemanticErrors result
                 then one $ SemanticErr path spans $ SA.semanticErrors result
                 else mempty
    pure (SA.semanticInfo result, errs)
  Typecheck path -> do
    (ast, _, spans) <- Rock.fetch (Parse path)
    case TS.typeCheck ast of
      Left err ->
        pure (mempty, one $ TypeErr path spans err)
      Right typeInfo ->
        pure (typeInfo, mempty)
  EmitDiagnostics path -> noError $ do
    -- Just triggering these tasks collects all the corresponding errors.
    _ <- Rock.fetch (Parse path)
    _ <- Rock.fetch (RunSemanticAnalysis path)
    _ <- Rock.fetch (Typecheck path)
    pass
  TransformAST path -> noError $ do
    -- Need to run SA and typechecking before any transformations / lowering
    -- to ensure we don't perform work on invalid programs.
    -- And thanks to rock, the results will be cached anyway.
    analysis <- Rock.fetch (RunSemanticAnalysis path)
    _ <- Rock.fetch (Typecheck path)
    -- Past this point, the code should be valid!
    -- We abort if this is not the case.
    abortOnError
    (ast, nodeId, _) <- Rock.fetch (Parse path)
    pure $ simplify nodeId analysis ast
  EmitSimplifiedAST path -> noError $ do
    (ast, _) <- Rock.fetch (TransformAST path)
    liftIO $ putTextLn $ printDoc ast
  StringMapping path -> noError $ do
    (_, mapping) <- Rock.fetch (TransformAST path)
    pure mapping
  UsageMapping path -> noError $ do
    (ast, _, _) <- Rock.fetch (Parse path)
    pure $ SA.computeUsageMapping ast
  CompileRA path -> noError $ do
    ast <- fst <$> Rock.fetch (TransformAST path)
    pure $ compileToRA ast
  EmitRA path -> noError $ do
    ra <- Rock.fetch (CompileRA path)
    liftIO $ putTextLn $ printDoc ra
  CompileEIR path -> noError $ do
    stringMapping <- Rock.fetch (StringMapping path)
    ra <- Rock.fetch (CompileRA path)
    typeInfo <- Rock.fetch (Typecheck path)
    pure $ compileToEIR stringMapping (TS.infoTypedefs typeInfo) ra
  EmitEIR path -> noError $ do
    eir <- Rock.fetch (CompileEIR path)
    liftIO $ putTextLn $ printDoc eir
  CompileLLVM path -> noError $ do
    eir <- Rock.fetch (CompileEIR path)
    stringMapping <- Rock.fetch (StringMapping path)
    usageMapping <- Rock.fetch (UsageMapping path)
    liftIO $ compileToLLVM (paramsConfig params) stringMapping usageMapping eir
  EmitLLVM path -> noError $ do
    llvmModule <- Rock.fetch (CompileLLVM path)
    liftIO $ putTextLn $ ppllvm llvmModule

-- Helper function for tasks that don't emit any errors.
noError :: Rock.Task Query a -> Rock.Task Query (a, [EclairError])
noError task =
  (, mempty) <$> task

type CompilerM a = IO (Either [EclairError] a)

data Aborted = Aborted
  deriving (Show, Exception)

runQuery :: Parameters -> Query a -> CompilerM a
runQuery params query = do
  memoVar <- newIORef mempty
  errRef <- newIORef mempty

  let onError :: q -> [EclairError] -> Rock.Task Query ()
      onError _ errs =
        liftIO $ modifyIORef errRef (<> errs)
      abortOnError = do
        errs <- readIORef errRef
        unless (null errs) $
          liftIO $ throwIO Aborted
      handleAbort =
        handle $ \Aborted -> do
          errs <- readIORef errRef
          pure $ Left errs
      task = Rock.fetch query

  handleAbort $ do
    result <- Rock.runTask (Rock.memoise memoVar $ Rock.writer onError $ rules abortOnError params) task
    errs <- readIORef errRef
    pure $ if null errs then Right result else Left errs

parse :: Parameters -> FilePath -> CompilerM (AST, SpanMap)
parse params =
  map (map (\(ast, _, spanMap) -> (ast, spanMap))) . runQuery params . Parse

semanticAnalysis :: Parameters -> FilePath -> CompilerM SA.SemanticInfo
semanticAnalysis params =
  runQuery params . RunSemanticAnalysis

typeCheck :: Parameters -> FilePath -> CompilerM TS.TypeInfo
typeCheck params =
  runQuery params . Typecheck

emitDiagnostics :: Parameters -> FilePath -> IO [EclairError]
emitDiagnostics params = do
  f <<$>> runQuery params . EmitDiagnostics
  where
    f = fromLeft mempty

emitSimplifiedAST :: Parameters -> FilePath -> CompilerM ()
emitSimplifiedAST params =
  runQuery params . EmitSimplifiedAST

emitRA :: Parameters -> FilePath -> CompilerM ()
emitRA params =
  runQuery params . EmitRA

emitEIR :: Parameters -> FilePath -> CompilerM ()
emitEIR params =
  runQuery params . EmitEIR

emitLLVM :: Parameters -> FilePath -> CompilerM ()
emitLLVM params =
  runQuery params . EmitLLVM
