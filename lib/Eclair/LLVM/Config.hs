module Eclair.LLVM.Config
  ( Config(..)
  , ConfigT
  , runConfigT
  , MonadConfig(..)
  ) where

import qualified LLVM.C.API as LibLLVM
import LLVM.Codegen
import Control.Monad.Morph
import Foreign.ForeignPtr
import Foreign.Ptr
import Eclair.ArgParser (Target)

-- This is a helper module for carrying around specific information
-- when compiling to a specific LLVM platform.

data Config
  = Config
    { cfgTargetTriple :: Maybe Target
    , cfgLLVMContext :: ForeignPtr LibLLVM.Context
    , cfgTargetData :: Ptr LibLLVM.TargetData
    }

-- TODO: automatically wrap ModuleBuilderT and call it CompileT?
newtype ConfigT m a
  = ConfigT
  { unConfigT :: ReaderT Config m a
  } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadModuleBuilder)
  via ReaderT Config m
  deriving MonadTrans via ReaderT Config

instance MFunctor ConfigT where
  hoist f =
    ConfigT . hoist f . unConfigT

runConfigT :: Config -> ConfigT m a -> m a
runConfigT cfg m =
  runReaderT (unConfigT m) cfg

class Monad m => MonadConfig m where
  getConfig :: m Config

instance Monad m => MonadConfig (ConfigT m) where
  getConfig =
    ConfigT ask

-- TODO other instances
