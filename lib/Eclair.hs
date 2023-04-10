{-# LANGUAGE GADTs, QuasiQuotes, TemplateHaskell, PackageImports, StandaloneDeriving #-}

module Eclair
  ( parse
  , semanticAnalysis
  , typeCheck
  , emitDiagnostics
  , emitTransformedAST
  , emitRA
  , emitTransformedRA
  , emitEIR
  , emitLLVM
  , emitSouffle
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
import Eclair.Common.Location
import Eclair.Common.Extern
import Eclair.Common.Config (Target(..))
import Eclair.AST.IR
import Eclair.AST.Transforms (StringMap)
import Eclair.Souffle.IR
import qualified Eclair.AST.Transforms as AST
import qualified Eclair.RA.IR as RA
import qualified Eclair.RA.Transforms as RA
import qualified Eclair.EIR.IR as EIR
import qualified Eclair.TypeSystem as TS
import qualified Eclair.AST.Analysis as SA
import LLVM.Codegen (Module, ppllvm)
import qualified Rock
import Data.GADT.Compare.TH (deriveGEq)
import "dependent-sum" Data.Some


type RA = RA.RA
type EIR = EIR.EIR


data Query a where
  Parse :: FilePath -> Query (AST, NodeId, SpanMap)
  RunSemanticAnalysis :: FilePath -> Query SA.SemanticInfo
  Typecheck :: FilePath -> Query TS.TypeInfo
  EmitDiagnostics :: FilePath -> Query ()
  TransformAST :: FilePath -> Query (AST, StringMap)
  EmitTransformedAST :: FilePath -> Query ()
  CompileRA :: FilePath -> Query RA
  TransformRA :: FilePath -> Query RA
  EmitRA :: FilePath -> Query ()
  EmitTransformedRA :: FilePath -> Query ()
  CompileEIR :: FilePath -> Query EIR
  EmitEIR :: FilePath -> Query ()
  CompileLLVM :: FilePath -> Query Module
  EmitLLVM :: FilePath -> Query ()
  EmitSouffle :: FilePath -> Query ()
  StringMapping :: FilePath -> Query (Map Text Word32)
  UsageMapping :: FilePath -> Query (Map Id UsageMode)
  ExternDefinitions :: FilePath -> Query [Extern]

deriving instance Eq (Query a)

queryFilePath :: Query a -> FilePath
queryFilePath = \case
  Parse path               -> path
  RunSemanticAnalysis path -> path
  Typecheck path           -> path
  EmitDiagnostics path     -> path
  TransformAST path        -> path
  EmitTransformedAST path  -> path
  CompileRA path           -> path
  TransformRA path         -> path
  EmitRA path              -> path
  EmitTransformedRA path   -> path
  CompileEIR path          -> path
  EmitEIR path             -> path
  CompileLLVM path         -> path
  EmitLLVM path            -> path
  EmitSouffle path         -> path
  StringMapping path       -> path
  UsageMapping path        -> path
  ExternDefinitions path   -> path

queryEnum :: Query a -> Int
queryEnum = \case
  Parse {}               -> 0
  RunSemanticAnalysis {} -> 1
  Typecheck {}           -> 2
  EmitDiagnostics {}     -> 3
  TransformAST {}        -> 4
  EmitTransformedAST {}  -> 5
  CompileRA {}           -> 6
  TransformRA {}         -> 7
  EmitRA {}              -> 8
  EmitTransformedRA {}   -> 9
  CompileEIR {}          -> 10
  EmitEIR {}             -> 11
  CompileLLVM {}         -> 12
  EmitLLVM {}            -> 13
  EmitSouffle {}         -> 14
  StringMapping {}       -> 15
  UsageMapping {}        -> 16
  ExternDefinitions {}   -> 17

deriveGEq ''Query

instance Hashable (Query a) where
  hashWithSalt salt =
    hashWithSalt salt . (queryFilePath &&& queryEnum)

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) =
    hashWithSalt salt query

data Parameters
  = Parameters
  { paramsNumCores :: Word
  , paramsTarget :: !(Maybe Target)
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
    result <- liftIO $ SA.runAnalysis (paramsNumCores params) ast
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
    externDefs <- Rock.fetch (ExternDefinitions path)
    pure $ AST.simplify nodeId externDefs analysis ast
  EmitTransformedAST path -> noError $ do
    (ast, _) <- Rock.fetch (TransformAST path)
    liftIO $ putTextLn $ printDoc ast
  StringMapping path -> noError $ do
    (_, mapping) <- Rock.fetch (TransformAST path)
    pure mapping
  UsageMapping path -> noError $ do
    (ast, _, _) <- Rock.fetch (Parse path)
    pure $ SA.computeUsageMapping ast
  ExternDefinitions path -> noError $ do
    (ast, _, _) <- Rock.fetch (Parse path)
    pure $ getExternDefs ast
  CompileRA path -> noError $ do
    ast <- fst <$> Rock.fetch (TransformAST path)
    externDefs <- Rock.fetch (ExternDefinitions path)
    pure $ compileToRA externDefs ast
  TransformRA path -> noError $ do
    ra <- Rock.fetch (CompileRA path)
    pure $ RA.simplify ra
  EmitRA path -> noError $ do
    ra <- Rock.fetch (CompileRA path)
    liftIO $ putTextLn $ printDoc ra
  EmitTransformedRA path -> noError $ do
    ra <- Rock.fetch (TransformRA path)
    liftIO $ putTextLn $ printDoc ra
  CompileEIR path -> noError $ do
    stringMapping <- Rock.fetch (StringMapping path)
    ra <- Rock.fetch (TransformRA path)
    typeInfo <- Rock.fetch (Typecheck path)
    pure $ compileToEIR stringMapping (TS.infoTypedefs typeInfo) ra
  EmitEIR path -> noError $ do
    eir <- Rock.fetch (CompileEIR path)
    liftIO $ putTextLn $ printDoc eir
  CompileLLVM path -> noError $ do
    eir <- Rock.fetch (CompileEIR path)
    stringMapping <- Rock.fetch (StringMapping path)
    usageMapping <- Rock.fetch (UsageMapping path)
    externDefs <- Rock.fetch (ExternDefinitions path)
    liftIO $ compileToLLVM (paramsTarget params) stringMapping usageMapping externDefs eir
  EmitLLVM path -> noError $ do
    llvmModule <- Rock.fetch (CompileLLVM path)
    liftIO $ putTextLn $ ppllvm llvmModule
  EmitSouffle path -> do
    (ast, _, _) <- Rock.fetch (Parse path)
    case toSouffle ast of
      Left err ->
        pure ((), one $ ConversionErr path err)
      Right souffleIR -> do
        liftIO $ putTextLn $ printDoc souffleIR
        pure ((), mempty)

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

emitTransformedAST :: Parameters -> FilePath -> CompilerM ()
emitTransformedAST params =
  runQuery params . EmitTransformedAST

emitRA :: Parameters -> FilePath -> CompilerM ()
emitRA params =
  runQuery params . EmitRA

emitTransformedRA :: Parameters -> FilePath -> CompilerM ()
emitTransformedRA params =
  runQuery params . EmitTransformedRA

emitEIR :: Parameters -> FilePath -> CompilerM ()
emitEIR params =
  runQuery params . EmitEIR

emitLLVM :: Parameters -> FilePath -> CompilerM ()
emitLLVM params =
  runQuery params . EmitLLVM

emitSouffle :: Parameters -> FilePath -> CompilerM ()
emitSouffle params =
  runQuery params . EmitSouffle
