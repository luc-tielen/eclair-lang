module Eclair.LSP.State
  ( HandlerM
  , Severity(..)
  , ServerConfig
  , liftLSP
  , readUri
  , fileFromUri
  ) where

import Control.Monad.Trans.Except (throwE)
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.VFS as LSP
import qualified Language.LSP.Types as LSP.Types
import qualified Data.Rope.UTF16 as Rope
import Language.LSP.Server (LspT)
import Language.LSP.Types

-- The exception layer allows us to fail gracefully, displaying a message to
-- the user via the "ShowMessage" mechanism of the lsp standard.
-- TODO add Rock's memovar as server state
type HandlerM = ExceptT (Severity, Text) (LspT ServerConfig IO)

type ServerConfig = ()

data Severity
  = Error   -- ^ Error displayed to the user.
  | Warning -- ^ Warning displayed to the user.
  | Info    -- ^ Information displayed to the user.
  | Log     -- ^ Log message, not displayed by default.

liftLSP :: LspT ServerConfig IO a -> HandlerM a
liftLSP = lift

-- | A helper function to query haskell-lsp's VFS.
readUri :: Uri -> HandlerM Text
readUri uri_ = do
  mVirtualFile <- liftLSP (LSP.getVirtualFile (LSP.Types.toNormalizedUri uri_))
  case mVirtualFile of
    Just (LSP.VirtualFile _ _ rope) ->
      pure (Rope.toText rope)
    Nothing ->
      throwE (Error, "Could not find " <> show uri_ <> " in VFS.")

fileFromUri :: Uri -> HandlerM FilePath
fileFromUri uri =
  case uriToFilePath uri of
    Just filePath ->
      pure filePath
    Nothing ->
      throwE (Error, getUri uri <> " is not a valid name for a Eclair file.")
