{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Eclair.LSP.Handlers
  ( initializedHandler
  , workspaceChangeConfigurationHandler
  , didOpenTextDocumentNotificationHandler
  , didSaveTextDocumentNotificationHandler
  , textDocumentChangeHandler
  , cancelationHandler
  ) where

import Data.Void    (Void)
import Eclair.LSP.State
import Control.Lens                     (assign, modifying, use, (^.))
import Control.Monad                    (forM, guard)
import Control.Monad.Trans.Except       (catchE, throwE)
import Data.Aeson                       (FromJSON(..), Value(..))
import Data.Text                        (Text, isPrefixOf)
import Language.LSP.Server              (Handlers, LspT)
import Language.LSP.Types               hiding (Range(..), line)
import Language.LSP.Types.Lens
import System.FilePath
import Text.Megaparsec                  (SourcePos (..), unPos)
import qualified Data.Aeson              as Aeson
import qualified Data.HashMap.Strict     as HashMap
import qualified Data.Map.Strict         as Map
import qualified Data.Rope.UTF16         as Rope
import qualified Data.Text               as Text
import qualified Language.LSP.Server     as LSP
import qualified Language.LSP.Types      as LSP.Types
import qualified Language.LSP.VFS        as LSP
import Eclair
import Eclair.ArgParser
import Eclair.LSP.Diagnostics


  {-
loadFile :: Uri -> HandlerM (Expr Src Void)
loadFile uri_ = do
  txt <- readUri uri_
  fileIdentifier <- fileIdentifierFromUri uri_
  cache <- use importCache

  expr <- case parse txt of
    Right e -> return e
    _ -> throwE (Error, "Failed to parse Dhall file.")

  loaded <- liftIO $ load fileIdentifier expr cache
  (cache', expr') <- case loaded of
    Right x -> return x
    _ -> throwE (Error, "Failed to resolve imports.")
  -- Update cache. Don't cache current expression because it might not have been
  -- written to disk yet (readUri reads from the VFS).
  assign importCache cache'
  return expr'

-- helper
rangeToJSON :: Range -> LSP.Types.Range
rangeToJSON (Range (x1,y1) (x2,y2)) =
    LSP.Types.Range
      (Position (fromIntegral x1) (fromIntegral y1))
      (Position (fromIntegral x2) (fromIntegral y2))

hoverHandler :: Handlers HandlerM
hoverHandler =
    LSP.requestHandler STextDocumentHover \request respond -> handleErrorWithDefault respond Nothing do
        let uri_ = request^.params.textDocument.uri

        let Position{ _line = fromIntegral -> _line, _character = fromIntegral -> _character } = request^.params.position

        errorMap <- use errors

        case Map.lookup uri_ errorMap of
            Nothing -> do
                expr <- loadFile uri_
                (welltyped, _) <- case typecheck expr of
                    Left  _  -> throwE (Info, "Can't infer type; code does not type-check.")
                    Right wt -> return wt
                case typeAt (_line, _character) welltyped of
                    Left err -> throwE (Error, Text.pack err)
                    Right (mSrc, typ) -> do
                        let _range = fmap (rangeToJSON . rangeFromDhall) mSrc

                        let _contents = HoverContents (MarkupContent MkPlainText (pretty typ))
                        respond (Right (Just Hover{ _contents, _range }))
            Just err -> do
                let isHovered (Diagnosis _ (Just (Range left right)) _) =
                        left <= (_line, _character) && (_line, _character) <= right
                    isHovered _ =
                        False

                let hoverFromDiagnosis (Diagnosis _ (Just (Range left right)) diagnosis) = do
                        let _range = Just (rangeToJSON (Range left right))
                            encodedDiag = URI.encode (Text.unpack diagnosis)

                            _kind = MkMarkdown

                            _value =
                                    "[Explain error](dhall-explain:?"
                                <>  Text.pack encodedDiag
                                <>  " )"

                            _contents = HoverContents MarkupContent{..}
                        Just Hover{ _contents, _range }
                    hoverFromDiagnosis _ =
                        Nothing

                let mHover = do
                        explanation <- explain err

                        guard (isHovered explanation)

                        hoverFromDiagnosis explanation

                respond (Right mHover)

documentLinkHandler :: Handlers HandlerM
documentLinkHandler =
    LSP.requestHandler STextDocumentDocumentLink \request respond -> handleErrorWithDefault respond (List []) do
        let uri_ = request^.params.textDocument.uri

        path <- case uriToFilePath uri_ of
            Nothing ->
                throwE (Log, "Could not process document links; failed to convert URI to file path.")
            Just p ->
                return p

        txt <- readUri uri_

        expr <- case parse txt of
            Right e ->
                return e
            Left _ ->
                throwE (Log, "Could not process document links; did not parse.")

        let imports = embedsWithRanges expr :: [(Range, Import)]

        let basePath = takeDirectory path

        let go :: (Range, Import) -> IO [DocumentLink]
            go (range_, Import (ImportHashed _ (Local prefix file)) _) = do
              filePath <- localToPath prefix file
              let filePath' = basePath </> filePath  -- absolute file path
              let _range = rangeToJSON range_
              let _target = Just (filePathToUri filePath')
              let _tooltip = Nothing
              let _xdata = Nothing
              return [DocumentLink {..}]

            go (range_, Import (ImportHashed _ (Remote url)) _) = do
              let _range = rangeToJSON range_
              let url' = url { headers = Nothing }
              let _target = Just (Uri (pretty url'))
              let _tooltip = Nothing
              let _xdata = Nothing
              return [DocumentLink {..}]

            go _ = return []

        links <- liftIO $ mapM go imports
        respond (Right (List (concat links)))


executeCommandHandler :: Handlers HandlerM
executeCommandHandler =
    LSP.requestHandler SWorkspaceExecuteCommand \request respond -> handleErrorWithDefault respond Aeson.Null do
        let command_ = request^.params.command
        if  | command_ == "dhall.server.lint" ->
                executeLintAndFormat request respond
            | command_ == "dhall.server.annotateLet" ->
                executeAnnotateLet request
            | command_ == "dhall.server.freezeImport" ->
                executeFreezeImport request
            | command_ == "dhall.server.freezeAllImports" ->
                executeFreezeAllImports request
            | otherwise -> do
                throwE
                    ( Warning
                    , "Command '" <> command_ <> "' not known; ignored."
                    )

getCommandArguments
-- (HasParams s a, FromJSON a) => s -> HandlerM a
getCommandArguments request = do
  json <- case request ^. params . arguments of
    Just (List (x : _)) -> return x
    _ -> throwE (Error, "Failed to execute command; arguments missing.")
  case Aeson.fromJSON json of
    Aeson.Success args ->
        return args
    _ ->
        throwE (Error, "Failed to execute command; failed to parse arguments.")

completionHandler :: Handlers HandlerM
completionHandler =
  LSP.requestHandler STextDocumentCompletion \request respond -> handleErrorWithDefault respond (InR (CompletionList False (List []))) do
    let uri_  = request ^. params . textDocument . uri
        line_ = fromIntegral (request ^. params . position . line)
        col_  = fromIntegral (request ^. params . position . character)

    txt <- readUri uri_
    let (completionLeadup, completionPrefix) = completionQueryAt txt (line_, col_)

    let computeCompletions
          -- environment variable
          | "env:" `isPrefixOf` completionPrefix =
            liftIO completeEnvironmentImport

          -- local import
          | any (`isPrefixOf` completionPrefix) [ "/", "./", "../", "~/" ] = do
            let relativeTo | Just path <- uriToFilePath uri_ = path
                         | otherwise = "."
            liftIO $ completeLocalImport relativeTo (Text.unpack completionPrefix)

          -- record projection / union constructor
          | (target_, _) <- Text.breakOnEnd "." completionPrefix
          , not (Text.null target_) = do
            let bindersExpr = binderExprFromText completionLeadup

            fileIdentifier <- fileIdentifierFromUri uri_
            cache <- use importCache
            loadedBinders <- liftIO $ load fileIdentifier bindersExpr cache

            (cache', bindersExpr') <-
              case loadedBinders of
                Right (cache', binders) ->
                  return (cache', binders)
                Left _ -> throwE (Log, "Could not complete projection; failed to load binders expression.")

            let completionContext = buildCompletionContext bindersExpr'

            targetExpr <- case parse (Text.dropEnd 1 target_) of
              Right e -> return e
              Left _ -> throwE (Log, "Could not complete projection; prefix did not parse.")

            loaded' <- liftIO $ load fileIdentifier targetExpr cache'
            case loaded' of
              Right (cache'', targetExpr') -> do
                assign importCache cache''
                return (completeProjections completionContext targetExpr')
              Left _ -> return []

          -- complete identifiers in scope
          | otherwise = do
            let bindersExpr = binderExprFromText completionLeadup

            fileIdentifier <- fileIdentifierFromUri uri_
            cache <- use importCache  -- todo save cache afterwards
            loadedBinders <- liftIO $ load fileIdentifier bindersExpr cache

            bindersExpr' <-
              case loadedBinders of
                Right (cache', binders) -> do
                  assign importCache cache'
                  return binders
                Left _ -> throwE (Log, "Could not complete projection; failed to load binders expression.")

            let context_ = buildCompletionContext bindersExpr'

            return (completeFromContext context_)

    completions <- computeCompletions

    let toCompletionItem (Completion {..}) = CompletionItem {..}
         where
          _label = completeText
          _kind = Nothing
          _tags = mempty
          _detail = fmap pretty completeType
          _documentation = Nothing
          _deprecated = Nothing
          _preselect = Nothing
          _sortText = Nothing
          _filterText = Nothing
          _insertText = Nothing
          _insertTextFormat = Nothing
          _insertTextMode = Nothing
          _textEdit = Nothing
          _additionalTextEdits = Nothing
          _commitCharacters = Nothing
          _command = Nothing
          _xdata = Nothing

    let _items = List (map toCompletionItem completions)

    let _isIncomplete = False

    respond (Right (InR CompletionList{..}))
-}

didOpenTextDocumentNotificationHandler :: Handlers HandlerM
didOpenTextDocumentNotificationHandler =
  LSP.notificationHandler STextDocumentDidOpen $ \notification -> do
    let _uri = notification^.params.textDocument.uri
    diagnosticsHandler _uri

didSaveTextDocumentNotificationHandler :: Handlers HandlerM
didSaveTextDocumentNotificationHandler =
  LSP.notificationHandler STextDocumentDidSave $ \notification -> do
    let _uri = notification^.params.textDocument.uri
    diagnosticsHandler _uri

diagnosticsHandler :: Uri -> HandlerM ()
diagnosticsHandler _uri = do
  file <- fileFromUri _uri
  let mTarget = Nothing
  errs <- liftIO $ emitDiagnostics mTarget file
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
textDocumentChangeHandler :: Handlers HandlerM
textDocumentChangeHandler =
  LSP.notificationHandler STextDocumentDidChange $ const pass

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
