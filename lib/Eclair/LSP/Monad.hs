module Eclair.LSP.Monad
  ( LspM
  , runLSP
  , liftLSP
  , getParams
  , module Eclair.LSP.VFS
  , posToOffset
  ) where

import Eclair (Parameters)
import Eclair.LSP.VFS
import Eclair.Common.Location
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

type LspM = VFST (ReaderT Parameters IO)

runLSP :: Parameters -> LspM a -> IO a
runLSP params m =
  runReaderT (runVFST m) params

getParams :: LspM Parameters
getParams = ask

liftLSP :: IO a -> LspM a
liftLSP = lift . lift

-- A hack to go from the LSP position to the offset in the file.
-- TODO check for off by 1 errors!
-- TODO move to Parser.hs
posToOffset :: SourcePos -> Text -> Either Text Int
posToOffset lspPos fileContents = do
  case P.runParser p "<lsp>" fileContents of
    Left _ ->
      Left "Error computing location offset in file."
    Right offset ->
      pure offset
  where
    p :: P.Parsec Void Text Int
    p = do
      -- Skip to correct line
      replicateM_ (fromIntegral $ sourcePosLine lspPos) $ do
        void $ P.takeWhileP Nothing (/= '\n')
        P.char '\n'
      -- Skip to correct column
      void $ P.takeP Nothing (fromIntegral $ sourcePosColumn lspPos)
      P.getOffset
