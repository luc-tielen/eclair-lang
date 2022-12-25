{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Eclair.LSP.Handlers
  ( initializedHandler
  , workspaceChangeConfigurationHandler
  , didOpenTextDocumentNotificationHandler
  , didChangeTextDocumentNotificationHandler
  , didSaveTextDocumentNotificationHandler
  , cancelationHandler
  , hoverHandler
  , documentHighlightHandler
  ) where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Control.Monad.Trans.Except (catchE, throwE)
import Language.LSP.Server (Handlers)
import Language.LSP.Types hiding (Range(..), line)
import Language.LSP.Types.Lens hiding (parameters)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Data.Map.Strict as Map
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP.Types
import Eclair
import Eclair.Error
import Eclair.Parser
import Eclair.Common.Pretty
import Eclair.TypeSystem (resolvedTypes)
import Eclair.LSP.Monad
import Eclair.LSP.Diagnostics
import Eclair.AST.IR


-- TODO implement completionHandler and documentLinkHandler (see Dhall LSP)

documentHighlightHandler :: Handlers HandlerM
documentHighlightHandler =
  LSP.requestHandler STextDocumentDocumentHighlight $ \request respond ->
    handleErrorWithDefault respond mempty $ do
      let uri_ = request ^. params . textDocument . uri
          pos = request ^. params . position
      file <- fileFromUri uri_
      parameters <- ask
      fileOffset <- posToOffset pos uri_
      parseResult <- liftIO $ runExceptT $ do
        (ast, spanMap) <- ExceptT (parse parameters file)
        let mNodeId = lookupNodeId spanMap fileOffset
        (ast, spanMap,) <$> liftEither (maybeToRight [] mNodeId)
      case parseResult of
        Left _errs ->
          throwE (Info, "Failed to get highlight information!")
        Right (ast, spanMap, nodeId) -> do
          fileContent <- readUri uri_
          let refs = findReferences ast nodeId
              highlights = getHighlights file fileContent spanMap refs

          respond (Right $ List highlights)
  where
    getHighlights file fileContent spanMap =
      map (\refNodeId ->
        let span' = lookupSpan spanMap refNodeId
            sourceSpan = spanToSourceSpan file fileContent span'
            _range = sourceSpanToLspRange sourceSpan
            _kind = Just HkText
        in DocumentHighlight {..})

-- TODO implement for concepts besides variables
findReferences :: AST -> NodeId -> [NodeId]
findReferences ast nodeId =
  fst <$> zygo getVarId getRefs ast
  where
    getVarId = \case
      PWildcardF {} ->
        -- Wildcard matches with nothing.
        mempty
      VarF varNodeId var | nodeId == varNodeId ->
        First (Just var)
      astf ->
        fold astf

    getRefs = \case
      ModuleF _ decls ->
        foldMap snd $ filter (isJust . getFirst . fst) decls

      RuleF _ _ args clauses -> do
        let subtrees = args <> clauses
         in case getFirst $ foldMap fst subtrees of
              Nothing -> mempty
              Just var ->
                filter ((== var) . snd) $ foldMap snd subtrees

      VarF varNodeId var ->
        [(varNodeId, var)]

      astf ->
        foldMap snd astf


hoverHandler :: Handlers HandlerM
hoverHandler =
  LSP.requestHandler STextDocumentHover $ \request respond -> handleErrorWithDefault respond Nothing $ do
    readSourceFile <- asks (map (map fromJust) . paramsReadSourceFile)
    let uri_ = request ^. params . textDocument . uri
        pos = request ^. params . position
    file <- fileFromUri uri_
    fileContent <- readUri uri_
    fileOffset <- posToOffset pos uri_

    -- TODO store cached info using Rock

    parameters <- ask
    tcResult <- liftIO $ runExceptT $ do
      (ast, spanMap) <- ExceptT (parse parameters file)
      (ast, spanMap,) <$> ExceptT (typeCheck parameters file)
    case tcResult of
      Left errs -> do
        issues <- mconcat <$> liftIO (traverse (errorToIssues readSourceFile) errs)
        let mIssue = flip find issues $ \i ->
              let loc = issueLocation i
                  (startPos, endPos) = (toPos $ locationStart loc, toPos $ locationEnd loc)
                in startPos <= pos && pos <= endPos
        case mIssue of
          Nothing ->
            throwE (Info, "File contains errors!")
          Just issue ->
            throwE (Info, renderIssueMessage file fileContent issue)

      Right (_, spanMap, typeInfo) -> do
        let maybeResult = do
              nodeId <- lookupNodeId spanMap fileOffset
              ty <- Map.lookup nodeId (resolvedTypes typeInfo)
              pure (nodeId, ty)
        case maybeResult of
          Nothing -> do
            throwE (Info, "No type information for this position!")
          Just (nodeId, ty) -> do
            let span' = lookupSpan spanMap nodeId
                sourceSpan = spanToSourceSpan file fileContent span'
                _range = Just $ sourceSpanToLspRange sourceSpan
                _contents = HoverContents (MarkupContent MkPlainText (printDoc ty))
            respond (Right (Just Hover{ _contents, _range }))
  where
    toPos pos =
      LSP.Types.Position (fromIntegral $ posLine pos)
                         (fromIntegral $ posColumn pos)

sourceSpanToLspRange :: SourceSpan -> LSP.Types.Range
sourceSpanToLspRange sourceSpan =
  let srcBegin = sourceSpanBegin sourceSpan
      srcEnd = sourceSpanEnd sourceSpan
      rangeStart = LSP.Types.Position (fromIntegral $ sourcePosLine srcBegin - 1) (fromIntegral $ sourcePosColumn srcBegin - 1)
      rangeEnd = LSP.Types.Position (fromIntegral $ sourcePosLine srcEnd - 1) (fromIntegral $ sourcePosColumn srcEnd - 1)
   in LSP.Types.Range rangeStart rangeEnd

-- A hack to go from the LSP position to the offset in the file.
posToOffset :: LSP.Types.Position -> Uri -> HandlerM Int
posToOffset pos uri' = do
  fileContent <- readUri uri'
  case P.runParser p "<lsp>" fileContent of
    Left _ ->
      throwE (Error, "Error computing location offset in file.")
    Right offset ->
      pure offset
  where
    p :: P.Parsec Void Text Int
    p = do
      -- Skip to correct line
      replicateM_ (fromIntegral $ _line pos) $ do
        void $ P.takeWhileP Nothing (/= '\n')
        P.char '\n'
      -- Skip to correct column
      void $ P.takeP Nothing (fromIntegral $ _character pos)
      P.getOffset

didOpenTextDocumentNotificationHandler :: Handlers HandlerM
didOpenTextDocumentNotificationHandler =
  LSP.notificationHandler STextDocumentDidOpen $ \notification -> do
    let _uri = notification ^. params . textDocument . uri
    diagnosticsHandler _uri

didChangeTextDocumentNotificationHandler :: Handlers HandlerM
didChangeTextDocumentNotificationHandler =
  LSP.notificationHandler STextDocumentDidChange $ \notification -> do
    let _uri = notification ^. params . textDocument . uri
    diagnosticsHandler _uri

didSaveTextDocumentNotificationHandler :: Handlers HandlerM
didSaveTextDocumentNotificationHandler =
  LSP.notificationHandler STextDocumentDidSave $ \notification -> do
    let _uri = notification ^. params . textDocument . uri
    diagnosticsHandler _uri

diagnosticsHandler :: Uri -> HandlerM ()
diagnosticsHandler _uri = do
  file <- fileFromUri _uri
  parameters <- ask

  errs <- liftIO $ emitDiagnostics parameters file
  -- TODO cache errors in server state?
  let _version = Nothing
  _diagnostics <- List . mconcat <$> traverse (errorToDiagnostics _uri) errs
  liftLSP (LSP.sendNotification STextDocumentPublishDiagnostics PublishDiagnosticsParams{ _uri, _version, _diagnostics })

-- This handler is a stub to prevent `lsp:no handler for:` messages.
initializedHandler :: Handlers HandlerM
initializedHandler =
  LSP.notificationHandler SInitialized $ const pass

-- This handler is a stub to prevent `lsp:no handler for:` messages.
workspaceChangeConfigurationHandler :: Handlers HandlerM
workspaceChangeConfigurationHandler =
  LSP.notificationHandler SWorkspaceDidChangeConfiguration $ const pass

-- This handler is a stub to prevent `lsp:no handler for:` messages.
cancelationHandler :: Handlers HandlerM
cancelationHandler =
  LSP.notificationHandler SCancelRequest $ const pass

handleErrorWithDefault :: (Either a1 b -> HandlerM a2) -> b -> HandlerM a2 -> HandlerM a2
handleErrorWithDefault respond _default =
  flip catchE handler
  where
    handler (Log, _message)  = do
      let _xtype = MtLog
      liftLSP $ LSP.sendNotification SWindowLogMessage LogMessageParams{..}
      respond (Right _default)

    handler (severity_, _message) = do
      let _xtype = case severity_ of
            Error   -> MtError
            Warning -> MtWarning
            Info    -> MtInfo

      liftLSP $ LSP.sendNotification SWindowShowMessage ShowMessageParams{..}
      respond (Right _default)
