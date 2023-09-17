module Eclair.LSP.Types
  ( Command(..)
  , Response(..)
  ) where

import Eclair.Common.Location
import Eclair.LSP.Handlers

data Command
  = Hover FilePath SourcePos
  | DocumentHighlight FilePath SourcePos
  | Diagnostics FilePath
  | UpdateVFS FilePath Text
  | Shutdown

data Response
  = HoverResponse HoverResult
  | DocumentHighlightResponse DocHLResult
  | DiagnosticsResponse DiagnosticsResult
  | SuccessResponse
  | ShuttingDown
