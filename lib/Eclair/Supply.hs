module Eclair.Supply
  ( SupplyT
  , Supply
  , runSupplyT
  , runSupply
  , fresh
  ) where

import Protolude


type Counter = Int

-- A monad for generating fresh values
newtype SupplyT m a
  = SupplyT (StateT Counter m a)
  deriving (Functor, Applicative, Monad, MonadReader r)
  via StateT Counter m

type Supply = SupplyT Identity

runSupplyT :: Monad m => SupplyT m a -> m a
runSupplyT (SupplyT m) =
  evalStateT m 0

runSupply :: Supply a -> a
runSupply = runIdentity . runSupplyT

fresh :: Monad m => SupplyT m Counter
fresh = SupplyT get
