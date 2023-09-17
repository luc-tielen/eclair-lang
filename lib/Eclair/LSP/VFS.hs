module Eclair.LSP.VFS
  ( VFS
  , VFST
  , runVFST
  , getVfsVar
  , vfsSetFile
  , vfsLookupFile
  , unsafeReadFromVFS
  ) where

import qualified Data.Map as M
import Data.Maybe (fromJust)

-- Virtual file system (some files might not be saved yet to disk!)
type VFS = M.Map FilePath Text

newtype VFST m a = VFST (ReaderT (MVar VFS) m a)
  deriving ( Functor, Applicative, Monad
           , MonadReader (MVar VFS), MonadIO)
  via ReaderT (MVar VFS) m

instance MonadTrans VFST where
  lift m =
    VFST $ lift m

runVFST :: MonadIO m => VFST m a -> m a
runVFST (VFST m) = do
  vfsVar <- liftIO $ newMVar mempty
  runReaderT m vfsVar

getVfsVar :: Monad m => VFST m (MVar VFS)
getVfsVar = ask

-- NOTE: Can be used both for adding and updating
vfsSetFile :: MonadIO m => FilePath -> Text -> VFST m ()
vfsSetFile path contents = do
  vfsVar <- getVfsVar
  liftIO $ modifyMVar_ vfsVar $
    pure . M.insert path contents

vfsLookupFile :: MonadIO m => FilePath -> VFST m (Maybe Text)
vfsLookupFile path = do
  vfsVar <- getVfsVar
  liftIO $ do
    vfs <- readMVar vfsVar
    pure $! M.lookup path vfs

-- Helper function used in a couple of handlers to directly read from VFS
-- Can be used as a drop-in replacement for "tryReadFile" in the compiler
-- pipeline once the VFS argument is applied.
unsafeReadFromVFS :: VFS -> FilePath -> IO Text
unsafeReadFromVFS vfs path =
  pure . fromJust $ M.lookup path vfs
