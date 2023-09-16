module Eclair.LSP
  ( lspMain
  ) where

import Eclair (Parameters(..))
import Eclair.Common.Location
import Eclair.Common.Pretty
import Eclair.TypeSystem hiding (typeCheck)
import Eclair.LSP.Handlers
import Eclair.LSP.Monad
import qualified Eclair.JSON as J
import qualified Data.Hermes as H
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

data NextStep
  = Continue
  | Stop

data Command
  = Hover FilePath SourcePos
  | DocumentHighlight FilePath SourcePos
  | Diagnostics FilePath
  | Shutdown

data Response
  = HoverResponse HoverResult
  | DocumentHighlightResponse DocHLResult
  | DiagnosticsResponse DiagnosticsResult
  | ShuttingDown

lspMain :: IO ()
lspMain = do
  env <- H.mkHermesEnv Nothing
  runLSP placeholderParams $ do
    vfs <- get
    -- TODO make numCores configurable via CLI
    -- TODO fix closing over empty vfs.. won't update! need MVar?
    let params = Parameters 0 Nothing (readFromVFS vfs)
    local (const params) $ go env
  where
    placeholderParams = Parameters 0 Nothing mempty
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
  Shutdown ->
    pure (Stop, ShuttingDown)

readCommand :: H.HermesEnv -> LspM (Either H.HermesException Command)
readCommand env = liftLSP $
  H.parseByteString env commandDecoder <$> BS.hGetLine stdin
  where
    commandDecoder :: H.Decoder Command
    commandDecoder = H.object $ do
      cmdType <- H.atKey "type" H.text
      let mDecoder = case cmdType of
            "hover" -> Just hoverDecoder
            "references" -> Just referencesDecoder
            "diagnostics" -> Just diagnosticsDecoder
            "shutdown" -> Nothing
            _ -> Nothing -- TODO return exception?
      case mDecoder of
        Nothing -> pure Shutdown
        Just decoder -> do
          H.atKey "command" decoder

    hoverDecoder = H.object $
      Hover
        <$> H.atKey "file" H.string
        -- <*> H.atKey "contents" H.text
        <*> H.atKey "position" srcPosDecoder

    referencesDecoder = H.object $
      DocumentHighlight
        <$> H.atKey "file" H.string
        -- <*> H.atKey "contents" H.text
        <*> H.atKey "position" srcPosDecoder

    diagnosticsDecoder = H.object $
      Diagnostics
        <$> H.atKey "file" H.string
        -- <*> H.atKey "contents" H.text

    srcPosDecoder = H.object $
      SourcePos
        <$> H.atKey "line" H.int
        <*> H.atKey "column" H.int

sendResponse :: Response -> LspM ()
sendResponse resp =
  liftLSP $ TIO.hPutStrLn stdout txt
  where
    txt = J.encodeJSON json
    json = case resp of
      HoverResponse (HoverOk srcSpan ty) ->
        J.Object $ HM.fromList
          [ ("location", srcSpanToJSON srcSpan)
          , ("type", typeToJSON ty)
          ]
      HoverResponse (HoverError path pos err) ->
        J.Object $ HM.fromList
          [ ("file", J.String $ toText path)
          , ("position", srcPosToJSON pos)
          , ("error", J.String err)
          ]
      DocumentHighlightResponse (DocHLOk refs) ->
        J.Array $ map srcSpanToJSON refs
      DocumentHighlightResponse (DocHLError path pos err) ->
        J.Object $ HM.fromList
          [ ("file", J.String $ toText path)
          , ("position", srcPosToJSON pos)
          , ("error", J.String err)
          ]
      DiagnosticsResponse (DiagnosticsOk diagnostics) ->
        J.Array $ map diagnosticToJSON diagnostics
      DiagnosticsResponse (DiagnosticsError path mPos err) ->
        J.Object $ HM.fromList
          [ ("file", J.String $ toText path)
          , ("position", srcPosToJSON $ fromMaybe (SourcePos 0 0) mPos)
          , ("error", J.String err)
          ]
      ShuttingDown ->
        J.Object $ HM.fromList [("shutdown", J.Boolean True)]

-- TODO move to separate file (together with decoders)
diagnosticToJSON :: Diagnostic -> J.JSON
diagnosticToJSON (Diagnostic source srcSpan severity msg) =
  J.Object $ HM.fromList
    [ ("location", srcSpanToJSON srcSpan)
    , ("source", diagnosticSourceToJSON source)
    , ("severity", severityToJSON severity)
    , ("message", J.String msg)
    ]

diagnosticSourceToJSON :: DiagnosticSource -> J.JSON
diagnosticSourceToJSON src =
  J.String $ show src

severityToJSON :: Severity -> J.JSON
severityToJSON Error =
  J.String "error"

srcSpanToJSON :: SourceSpan -> J.JSON
srcSpanToJSON srcSpan =
  J.Object $ HM.fromList
    [ ("file", J.String $ toText path)
    , ("start", J.Object $ HM.fromList
        [ ("line", J.Number $ sourcePosLine start)
        , ("column", J.Number $ sourcePosColumn start)
        ]
      )
    , ("end", J.Object $ HM.fromList
        [ ("line", J.Number $ sourcePosLine end)
        , ("column", J.Number $ sourcePosColumn end)
        ]
      )
    ]
  where
    path = sourceSpanFile srcSpan
    start = sourceSpanBegin srcSpan
    end = sourceSpanEnd srcSpan

srcPosToJSON :: SourcePos -> J.JSON
srcPosToJSON pos =
  J.Object $ HM.fromList
    [ ("line", J.Number $ sourcePosLine pos)
    , ("column", J.Number $ sourcePosColumn pos)
    ]

typeToJSON :: Type -> J.JSON
typeToJSON =
  J.String . printDoc
