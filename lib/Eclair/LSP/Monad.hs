module Eclair.LSP.Monad
  ( LspM
  , runLSP
  , liftLSP
  , getParams
  , module Eclair.LSP.VFS
  , posToOffset
  , toMachineSrcPos
  ) where

import Eclair (Parameters(..))
import Eclair.LSP.VFS
import Eclair.Common.Location
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

type LspM = ReaderT Parameters (VFST IO)

runLSP :: LspM a -> IO a
runLSP m = do
  runVFST $ runReaderT m placeHolderParams
  where
    -- TODO make number of cores configurable via CLI
    placeHolderParams = Parameters 1 Nothing mempty

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
      Left "Error computing location offset in file!"
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

-- Helper function to go from 1-indexing to 0-indexing
toMachineSrcPos :: SourcePos -> SourcePos
toMachineSrcPos srcPos =
  SourcePos (sourcePosLine srcPos - 1) (sourcePosColumn srcPos - 1)
