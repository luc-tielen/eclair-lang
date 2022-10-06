{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}

module Eclair.LLVM.Template
  ( TemplateT
  , Template
  , HasSuffix(..)
  , MonadTemplate(..)
  , Suffix
  , cmapParams
  , instantiate
  , function
  , typedef
  ) where


import Control.Monad.Morph
import LLVM.Codegen hiding (function, typedef)
import qualified LLVM.Codegen as CG
import qualified Control.Monad.State.Lazy as LazyState
import qualified Control.Monad.State.Strict as StrictState
import qualified Control.Monad.RWS.Lazy as LazyRWS
import qualified Control.Monad.RWS.Strict as StrictRWS


type Suffix = Text

-- | A MTL-like monad transformer that allows generating code in a way similar to C++ templates.
-- Instead of complicated machinery in the compiler, this transformer just adds a suffix to all generated functions and types.
-- It is up to the programmer to make sure all provided suffixes to one template are unique!
-- The type variable 'p' is short for "template parameters" and can be used to tweak (specialize) the code generation.
-- The type variable 'm' allows running this stack in a pure context, or in a stack on top of IO.
newtype TemplateT p m a
  = TemplateT
  { unTemplateT :: ReaderT (Suffix, p) (ModuleBuilderT m) a
  } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadError e, MonadState s, MonadModuleBuilder)
  via ReaderT (Suffix, p) (ModuleBuilderT m)

type Template p = TemplateT p Identity

instance MFunctor (TemplateT p) where
  hoist nat = TemplateT . hoist (hoist nat) . unTemplateT

instance MonadReader r m => MonadReader r (TemplateT p m) where
  ask = lift ask
  local f (TemplateT m) =
    TemplateT $ hoist (local f) m

class HasSuffix m where
  getSuffix :: m Suffix

instance Monad m => HasSuffix (TemplateT p m) where
  getSuffix = TemplateT $ asks (("_" <>) . fst)
-- The following instance makes 'function' behave the same as in llvm-codegen
instance Monad m => HasSuffix (ModuleBuilderT m) where
  getSuffix = pure mempty
-- This allows getting the suffix inside a function body with llvm-codegen
instance (Monad m, HasSuffix m) => HasSuffix (IRBuilderT m) where
  getSuffix = lift getSuffix
-- MTL boilerplate:
instance (Monad m, HasSuffix m) => HasSuffix (ReaderT r m) where
  getSuffix = lift getSuffix
instance (Monad m, HasSuffix m, Monoid w) => HasSuffix (WriterT w m) where
  getSuffix = lift getSuffix
instance (Monad m, HasSuffix m, Monoid w) => HasSuffix (LazyState.StateT w m) where
  getSuffix = lift getSuffix
instance (Monad m, HasSuffix m, Monoid w) => HasSuffix (StrictState.StateT w m) where
  getSuffix = lift getSuffix
instance (Monad m, HasSuffix m, Monoid w) => HasSuffix (LazyRWS.RWST r w s m) where
  getSuffix = lift getSuffix
instance (Monad m, HasSuffix m, Monoid w) => HasSuffix (StrictRWS.RWST r w s m) where
  getSuffix = lift getSuffix
instance (Monad m, HasSuffix m) => HasSuffix (ExceptT e m) where
  getSuffix = lift getSuffix

class MonadTemplate p m | m -> p where
  getParams :: m p

instance Monad m => MonadTemplate p (TemplateT p m) where
  getParams = TemplateT $ asks snd

instance (Monad m, MonadTemplate p m) => MonadTemplate p (ReaderT r m) where
  getParams = lift getParams
instance (Monad m, MonadTemplate p m, Monoid w) => MonadTemplate p (WriterT w m) where
  getParams = lift getParams
instance (Monad m, MonadTemplate p m, Monoid w) => MonadTemplate p (LazyState.StateT w m) where
  getParams = lift getParams
instance (Monad m, MonadTemplate p m, Monoid w) => MonadTemplate p (StrictState.StateT w m) where
  getParams = lift getParams
instance (Monad m, MonadTemplate p m, Monoid w) => MonadTemplate p (LazyRWS.RWST r w s m) where
  getParams = lift getParams
instance (Monad m, MonadTemplate p m, Monoid w) => MonadTemplate p (StrictRWS.RWST r w s m) where
  getParams = lift getParams
instance (Monad m, MonadTemplate p m) => MonadTemplate p (ExceptT e m) where
  getParams = lift getParams
-- This allows getting the params inside a function body with llvm-codegen
instance (Monad m, MonadTemplate p m) => MonadTemplate p (IRBuilderT m) where
  getParams = lift getParams

instance MonadTrans (TemplateT p) where
  lift m =
    TemplateT $ ReaderT $ const $ lift m

-- "contramap" over the template params.
-- Useful if you only need access to part of the template data
-- and/or types of the params don't match.
cmapParams :: (p2 -> p1) -> TemplateT p1 m a -> TemplateT p2 m a
cmapParams f (TemplateT m) =
  TemplateT $ flip withReaderT m $ second f

-- This instantiates a template, given a template name suffix and some template parameters.
-- The result is the underlying ModuleBuilerT which generates specialized code based on the parameters.
instantiate :: Suffix -> p -> TemplateT p m a -> ModuleBuilderT m a
instantiate suffix p (TemplateT t) =
  runReaderT t (suffix, p)

-- The next functions replace the corresponding functions defined in llvm-codegen.
-- The functions automatically add a suffix if needed, to guarantee unique function names.
-- In the actual codegen, you will still need to call 'getParams' to get access to the params,
-- to do the actual specialization based on them.

function :: (MonadModuleBuilder m, HasSuffix m)
         => Name -> [(Type, ParameterName)] -> Type -> ([Operand] -> IRBuilderT m a) -> m Operand
function (Name name) args retTy body = do
  suffix <- getSuffix
  let nameWithSuffix = Name $ name <> suffix
  CG.function nameWithSuffix args retTy body

typedef :: (MonadModuleBuilder m, HasSuffix m)
        => Name -> Flag Packed -> [Type] -> m Type
typedef (Name name) packedFlag tys = do
  suffix <- getSuffix
  let nameWithSuffix = Name $ name <> suffix
  CG.typedef nameWithSuffix packedFlag tys
