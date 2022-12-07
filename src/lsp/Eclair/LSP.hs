{-# LANGUAGE RecordWildCards #-}

module Eclair.LSP
  ( run
  ) where

-- The code in this module all the modules in Eclair.LSP.* are heavily based
-- on the Dhall LSP implementation: https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-lsp-server

import Data.Aeson (fromJSON)
import Data.Default
import System.Exit (ExitCode(..))
import Language.LSP.Server (Options(..), ServerDefinition(..), type (<~>)(..))
import Language.LSP.Types
import qualified Data.Aeson as Aeson
import qualified Language.LSP.Server as LSP
import qualified System.Exit as Exit
import qualified System.Log.Logger
import Eclair.LSP.Handlers
    ( didOpenTextDocumentNotificationHandler
    , didChangeTextDocumentNotificationHandler
    , didSaveTextDocumentNotificationHandler
    , hoverHandler
    , initializedHandler
    , workspaceChangeConfigurationHandler
    , textDocumentChangeHandler
    , cancelationHandler
    )
import Eclair.LSP.Monad
import Eclair

-- | The main entry point for the LSP server.
run :: Maybe FilePath -> IO ()
run mLogFile = do
  setupLogger mLogFile

  -- TODO initialize rock IORef

  let defaultConfig = def
      onConfigurationChange _oldConfig json =
        case fromJSON json of
          Aeson.Success config ->
            Right config
          Aeson.Error   string ->
            Left (toText string)

      doInitialize environment _request = do
        pure (Right environment)
      options = def
        { LSP.textDocumentSync = Just syncOptions
        , completionTriggerCharacters = Nothing   -- TODO enable again when we add a completionHandler
        , executeCommandCommands = Nothing
        }
      staticHandlers =
        mconcat
          [ didOpenTextDocumentNotificationHandler
          , didChangeTextDocumentNotificationHandler
          , didSaveTextDocumentNotificationHandler
          , initializedHandler
          , workspaceChangeConfigurationHandler
          , textDocumentChangeHandler
          , cancelationHandler
          , hoverHandler
          ]

      interpretHandler environment = Iso{..}
        where
          backward = liftIO

          forward :: HandlerM a -> IO a
          forward handler = do
            -- TODO modifyIORef state
            LSP.runLspT environment $ do
              -- This should work, since the VFS is wrapped by a TVar
              let tryReadFile = lspReadFromVFS environment
                  params = Parameters Nothing tryReadFile
              runReaderT (runExceptT handler) params >>= \case
                Left (Log, _message) -> do
                  let _xtype = MtLog
                  LSP.sendNotification SWindowLogMessage LogMessageParams{..}
                  liftIO (fail (toString _message))

                Left (severity_, _message) -> do
                  let _xtype = case severity_ of
                        Error   -> MtError
                        Warning -> MtWarning
                        Info    -> MtInfo

                  LSP.sendNotification SWindowShowMessage ShowMessageParams{..}
                  liftIO (fail (toString _message))
                Right a -> do
                  pure a

  LSP.runServer ServerDefinition{..} >>= \case
    0 ->
      pass
    n ->
      Exit.exitWith (ExitFailure n)


-- Tells the LSP client to notify us about file changes. Handled behind the
-- scenes by haskell-lsp (in Language.Haskell.LSP.VFS); we don't handle the
-- corresponding notifications ourselves.
syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose         = Just True
  , _change            = Just TdSyncIncremental
  , _willSave          = Just False
  , _willSaveWaitUntil = Just False
  , _save              = Just (InR (SaveOptions (Just False)))
  }

setupLogger :: Maybe FilePath -> IO ()
setupLogger = \case
  Nothing ->
    pass
  Just "[OUTPUT]" ->
    LSP.setupLogger Nothing [] System.Log.Logger.DEBUG
  Just file ->
    LSP.setupLogger (Just file) [] System.Log.Logger.DEBUG
