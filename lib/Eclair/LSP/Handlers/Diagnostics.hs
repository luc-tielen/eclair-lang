module Eclair.LSP.Handlers.Diagnostics
  ( diagnosticsHandler
  , DiagnosticSource(..)
  , Severity(..)
  , Diagnostic(..)
  , DiagnosticsResult(..)
  ) where

import Eclair
import Eclair.LSP.Monad
import Eclair.Common.Location
import Eclair.Error

data DiagnosticSource
  = Parser
  | Typesystem
  | SemanticAnalysis
  | Transpiler
  deriving (Eq, Show)

data Severity
  = Error  -- (for now Eclair only has errors)
  deriving (Eq, Show)

data Diagnostic
  = Diagnostic DiagnosticSource SourceSpan Severity Text
  deriving (Eq, Show)

data DiagnosticsResult
  = DiagnosticsOk [Diagnostic]
  | DiagnosticsError FilePath (Maybe SourcePos) Text
  deriving (Eq, Show)

diagnosticsHandler :: FilePath -> LspM DiagnosticsResult
diagnosticsHandler path = do
  params <- getParams
  mFileContents <- lift $ vfsLookupFile path
  case mFileContents of
    Nothing ->
      pure $ DiagnosticsError path Nothing "File not found in VFS!"
    Just _fileContents -> do
      errs <- liftLSP $ emitDiagnostics params path
      diagnostics <- mconcat <$> traverse errorToDiagnostics errs
      pure $ DiagnosticsOk diagnostics
  where
    errorToDiagnostics :: EclairError -> LspM [Diagnostic]
    errorToDiagnostics err = do
      vfsVar <- lift getVfsVar
      vfs <- liftIO $ readMVar vfsVar
      let readSourceFile = unsafeReadFromVFS vfs
          source = diagnosticSource err
      issues <- liftLSP $ errorToIssues readSourceFile err
      traverse (toDiagnostic source) issues

    toDiagnostic source issue = do
      let msg = renderIssueMessage issue
          srcSpan = locationToSourceSpan $ issueLocation issue
      pure $ Diagnostic source srcSpan Error msg

    diagnosticSource :: EclairError -> DiagnosticSource
    diagnosticSource = \case
      ParseErr {} -> Parser
      TypeErr {} -> Typesystem
      SemanticErr {} -> SemanticAnalysis
      ConversionErr {} -> Transpiler
