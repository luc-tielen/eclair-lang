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
  deriving Show

data Severity
  = Error  -- (for now Eclair only has errors)

data Diagnostic
  = Diagnostic DiagnosticSource SourceSpan Severity Text

data DiagnosticsResult
  = DiagnosticsOk [Diagnostic]
  | DiagnosticsError FilePath (Maybe SourcePos) Text

diagnosticsHandler :: FilePath -> LspM DiagnosticsResult
diagnosticsHandler path = do
  params <- getParams
  mFileContents <- lift $ vfsLookupFile path
  case mFileContents of
    Nothing ->
      pure $ DiagnosticsError path Nothing "Failed to read file from VFS!"
    Just fileContents -> do
      errs <- liftLSP $ emitDiagnostics params path
      diagnostics <- mconcat <$> traverse (errorToDiagnostics fileContents) errs
      pure $ DiagnosticsOk diagnostics
  where
    errorToDiagnostics :: Text -> EclairError -> LspM [Diagnostic]
    errorToDiagnostics fileContents err = do
      vfsVar <- lift getVfsVar
      vfs <- liftIO $ readMVar vfsVar
      let readSourceFile = unsafeReadFromVFS vfs
          source = diagnosticSource err
      issues <- liftLSP $ errorToIssues readSourceFile err
      traverse (toDiagnostic fileContents source) issues

    toDiagnostic fileContents source issue = do
      let msg = renderIssueMessage path fileContents issue
          srcSpan = locationToSourceSpan $ issueLocation issue
      pure $ Diagnostic source srcSpan Error msg

    diagnosticSource :: EclairError -> DiagnosticSource
    diagnosticSource = \case
      ParseErr {} -> Parser
      TypeErr {} -> Typesystem
      SemanticErr {} -> SemanticAnalysis
      ConversionErr {} -> Transpiler
