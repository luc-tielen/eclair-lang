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
import qualified Data.HashMap.Strict as HM

commandDecoder :: H.Decoder Command
commandDecoder = H.object $ do
  cmdType <- H.atKey "type" H.text
  let mDecoder = case cmdType of
        "hover" -> Just hoverDecoder
        "references" -> Just referencesDecoder
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

responseToJSON :: Response -> J.JSON
responseToJSON = \case
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
  SuccessResponse ->
    J.Object $ HM.fromList [("success", J.Boolean True)]
  ShuttingDown ->
    J.Object $ HM.fromList [("shutdown", J.Boolean True)]

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
