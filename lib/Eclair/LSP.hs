module Eclair.LSP
  ( lspMain
  ) where

import Eclair (Parameters(..))
import Eclair.LSP.Handlers
import Eclair.LSP.Monad
import Eclair.LSP.Types
import Eclair.LSP.JSON
import qualified Eclair.JSON as J
import qualified Data.Hermes as H
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import qualified Data.Map as M

data NextStep
  = Continue
  | Stop

lspMain :: IO ()
lspMain = do
  env <- H.mkHermesEnv Nothing
  runLSP $ do
    vfsVar <- lift getVfsVar
    let readVFS path = do
          vfs <- readMVar vfsVar
          pure $! M.lookup path vfs

    -- TODO make numCores configurable via CLI
    let params = Parameters 1 Nothing readVFS
    local (const params) $ go env
  where
    go env = readCommand env >>= \case
      Left _err ->
        sendResponse ShuttingDown
      Right command -> do
        (nextStep, result) <- processCommand command
        sendResponse result
        case nextStep of
          Continue -> go env
          Stop -> pass

processCommand :: Command -> LspM (NextStep, Response)
processCommand = \case
  Hover path srcPos -> do
    result <- hoverHandler path srcPos
    pure (Continue, HoverResponse result)
  DocumentHighlight path srcPos -> do
    hls <- documentHighlightHandler path srcPos
    pure (Continue, DocumentHighlightResponse hls)
  Diagnostics path -> do
    diagnostics <- diagnosticsHandler path
    pure (Continue, DiagnosticsResponse diagnostics)
  UpdateVFS path fileContents -> do
    lift $ vfsSetFile path fileContents
    pure (Continue, SuccessResponse)
  Shutdown ->
    pure (Stop, ShuttingDown)

readCommand :: H.HermesEnv -> LspM (Either H.HermesException Command)
readCommand env = liftLSP $
  H.parseByteString env commandDecoder <$> BS.hGetLine stdin

sendResponse :: Response -> LspM ()
sendResponse resp =
  liftLSP $ TIO.hPutStrLn stdout txt
  where
    txt = J.encodeJSON json
    json = responseToJSON resp
