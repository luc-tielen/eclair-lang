module Eclair.LSP.VFS
  ( VFS
  , VFST
  , runVFST
  , vfsSetFile
  , vfsLookupFile
  , readFromVFS
  , unsafeReadFromVFS
  ) where

import qualified Data.Map as M
import Data.Maybe (fromJust)

type VFS = M.Map FilePath Text

newtype VFST m a = VFST (StateT VFS m a)
  deriving (Functor, Applicative, Monad)
  via StateT VFS m
  deriving (MonadState VFS, MonadReader r)
  via StateT VFS m

instance MonadTrans VFST where
  lift m =
    VFST $ lift m

runVFST :: Monad m => VFST m a -> m a
runVFST (VFST m) =
  evalStateT m mempty

-- NOTE: Can be used both for adding and updating
vfsSetFile :: Monad m => FilePath -> Text -> VFST m ()
vfsSetFile path contents =
  modify' $! M.insert path contents

vfsLookupFile :: Monad m => FilePath -> VFST m (Maybe Text)
vfsLookupFile path =
  gets $ M.lookup path

-- Helper function used to directly read from the VFS
readFromVFS :: VFS -> FilePath -> IO (Maybe Text)
readFromVFS vfs =
  pure . flip M.lookup vfs

-- Helper function used in a couple of handlers to directly read from VFS
-- Can be used as a drop-in replacement for "tryReadFile" in the compiler
-- pipeline once the VFS argument is applied.
unsafeReadFromVFS :: VFS -> FilePath -> IO Text
unsafeReadFromVFS vfs path =
  fromJust <$> readFromVFS vfs path
