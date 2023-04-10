{-# LANGUAGE RecordWildCards #-}
module Eclair.LSP.Diagnostics
  ( errorToDiagnostics
  ) where

import Language.LSP.Types hiding (Range(..), Location(..), ParseError, Position, line)
import qualified Language.LSP.Types as LSP
import Language.LSP.Server (getLspEnv)
import Eclair.Error
import Eclair.LSP.Monad
import Data.Maybe (fromJust)


errorToDiagnostics :: Uri -> EclairError -> HandlerM [Diagnostic]
errorToDiagnostics uri err = do
  file <- fileFromUri uri
  fileContent <- readUri (filePathToUri file)
  env <- lift getLspEnv
  let readSourceFile = map (map fromJust) $ lspReadFromVFS env
  issues <- liftIO $ errorToIssues readSourceFile err
  let source = errorSource err
  pure $ map (toDiagnostic source file fileContent) issues
  where
    toDiagnostic source file fileContent issue =
      mkDiagnostic (renderIssueMessage file fileContent issue) (issueRange issue) source

    issueRange issue =
      Just (locationToLspRange $ issueLocation issue)

data Source
  = Parser
  | Typesystem
  | SemanticAnalysis
  | Transpiler

sourceToText :: Source -> Text
sourceToText = \case
  Parser -> "Eclair.Parser"
  Typesystem -> "Eclair.Typesystem"
  SemanticAnalysis -> "Eclair.SemanticAnalysis"
  Transpiler -> "Eclair.Souffle.IR"

errorSource :: EclairError -> Source
errorSource = \case
  ParseErr {} -> Parser
  TypeErr {} -> Typesystem
  SemanticErr {} -> SemanticAnalysis
  ConversionErr {} -> Transpiler

mkDiagnostic :: Text -> Maybe LSP.Range -> Source -> Diagnostic
mkDiagnostic message location source =
  let _range = fromMaybe nullRange location
      _severity = Just DsError
      _source = Just $ sourceToText source
      _code = Nothing
      _tags = Nothing
      _message = message
      _relatedInformation = Nothing
   in Diagnostic {..}

nullRange :: LSP.Range
nullRange =
  LSP.Range (LSP.Position 0 0) (LSP.Position 0 0)

-- Helper conversion function. Eclair has a separate type so it does not need
-- to rely on the LSP library.
locationToLspRange :: Location -> LSP.Range
locationToLspRange loc =
  LSP.Range (LSP.Position (fromIntegral $ posLine start) (fromIntegral $ posColumn start))
            (LSP.Position (fromIntegral $ posLine end) (fromIntegral $ posColumn end))
  where
    start = locationStart loc
    end = locationEnd loc
