module Eclair.LSP.JSON
  ( responseToJSON
  , diagnosticToJSON
  , diagnosticSourceToJSON
  , severityToJSON
  , srcSpanToJSON
  , srcPosToJSON
  , typeToJSON
  , commandDecoder
  , hoverDecoder
  , referencesDecoder
  , diagnosticsDecoder
  , updateVfsDecoder
  , srcPosDecoder
  ) where

import qualified Eclair.JSON as J
import qualified Data.Hermes as H
import Eclair.Common.Pretty
import Eclair.TypeSystem hiding (typeCheck)
import Eclair.LSP.Handlers
import Eclair.LSP.Types
import Eclair.Common.Location

commandDecoder :: H.Decoder Command
commandDecoder = H.object $ do
  cmdType <- H.atKey "type" H.text
  let mDecoder = case cmdType of
        "hover" -> Just hoverDecoder
        "document-highlight" -> Just referencesDecoder
        "diagnostics" -> Just diagnosticsDecoder
        "update-vfs" -> Just updateVfsDecoder
        "shutdown" -> Nothing
        _ -> Nothing -- TODO return exception?
  case mDecoder of
    Nothing -> pure Shutdown
    Just decoder -> do
      H.atKey "command" decoder

hoverDecoder :: H.Decoder Command
hoverDecoder = H.object $
  Hover
    <$> H.atKey "file" H.string
    <*> H.atKey "position" srcPosDecoder

referencesDecoder :: H.Decoder Command
referencesDecoder = H.object $
  DocumentHighlight
    <$> H.atKey "file" H.string
    <*> H.atKey "position" srcPosDecoder

diagnosticsDecoder :: H.Decoder Command
diagnosticsDecoder = H.object $
  Diagnostics
    <$> H.atKey "file" H.string

updateVfsDecoder :: H.Decoder Command
updateVfsDecoder = H.object $
  UpdateVFS
    <$> H.atKey "file" H.string
    <*> H.atKey "contents" H.text

srcPosDecoder :: H.Decoder SourcePos
srcPosDecoder = H.object $
  SourcePos
    <$> H.atKey "line" H.int
    <*> H.atKey "column" H.int

successResponse :: Text -> J.JSON -> J.JSON
successResponse responseKey response =
  J.Object [
    ("type", J.String "success"),
    (responseKey, response)
  ]

errorResponse :: J.JSON -> J.JSON
errorResponse response =
  J.Object [
    ("type", J.String "error"),
    ("error", response)
  ]

responseToJSON :: Response -> J.JSON
responseToJSON = \case
  HoverResponse (HoverOk srcSpan ty) ->
    successResponse "hover" $ J.Object
      [ ("location", srcSpanToJSON srcSpan)
      , ("type", typeToJSON ty)
      ]
  HoverResponse (HoverError path pos err) ->
    errorResponse $ J.Object
      [ ("file", J.String $ toText path)
      , ("position", srcPosToJSON pos)
      , ("message", J.String err)
      ]
  DocumentHighlightResponse (DocHLOk refs) ->
    successResponse "highlights" $ J.Array $ map srcSpanToJSON refs
  DocumentHighlightResponse (DocHLError path pos err) ->
    errorResponse $ J.Object
      [ ("file", J.String $ toText path)
      , ("position", srcPosToJSON pos)
      , ("message", J.String err)
      ]
  DiagnosticsResponse (DiagnosticsOk diagnostics) ->
    successResponse "diagnostics" $ J.Array $ map diagnosticToJSON diagnostics
  DiagnosticsResponse (DiagnosticsError path mPos err) ->
    errorResponse $ J.Object
      [ ("file", J.String $ toText path)
      , ("position", srcPosToJSON $ fromMaybe (SourcePos 0 0) mPos)
      , ("message", J.String err)
      ]
  SuccessResponse ->
    J.Object [("success", J.Boolean True)]
  ShuttingDown ->
    J.Object [("shutdown", J.Boolean True)]

diagnosticToJSON :: Diagnostic -> J.JSON
diagnosticToJSON (Diagnostic source srcSpan severity msg) =
  J.Object
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
  J.Object
    [ ("file", J.String $ toText path)
    , ("start", J.Object
        [ ("line", J.Number $ sourcePosLine start)
        , ("column", J.Number $ sourcePosColumn start)
        ]
      )
    , ("end", J.Object
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
  J.Object
    [ ("line", J.Number $ sourcePosLine pos)
    , ("column", J.Number $ sourcePosColumn pos)
    ]

typeToJSON :: Type -> J.JSON
typeToJSON =
  J.String . printDoc
