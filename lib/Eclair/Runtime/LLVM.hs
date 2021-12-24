{-# LANGUAGE RoleAnnotations, PolyKinds #-}

module Eclair.Runtime.LLVM
  ( module Eclair.Runtime.LLVM
  ) where

import Protolude hiding ( Type, (.), bit )
import Control.Category
import Control.Monad.Morph
import Control.Monad.Fix
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Constant as Constant
import LLVM.AST.Operand ( Operand(..) )
import LLVM.AST.Type
import LLVM.AST.Name
import Eclair.Runtime.Hash
import LLVM.Internal.EncodeAST
import LLVM.Internal.Coding hiding (alloca)
import LLVM.Internal.Type
import qualified LLVM.Context as Context
import qualified LLVM.Internal.DataLayout as DL
import qualified LLVM.Internal.FFI.DataLayout as DL
import qualified LLVM.CodeGenOpt as CG
import qualified LLVM.CodeModel as CM
import qualified LLVM.Relocation as Rel
import LLVM.Target


-- TODO: remove, import directly from llvm-hs
int16 :: Integer -> Operand
int16 = ConstantOperand . Constant.Int 16

nullPtr :: Type -> Operand
nullPtr = ConstantOperand . Constant.Null . ptr

def :: (MonadModuleBuilder m, MonadReader r m, ToHash r)
    => Text
    -> [(Type, ParameterName)]
    -> Type
    -> ([Operand] -> IRBuilderT m ())
    -> m Operand
def funcName args retTy body = do
  h <- asks getHash
  let funcNameWithHash = mkName $ T.unpack $ funcName <> "_" <> unHash h
  function funcNameWithHash args retTy body

mkType :: (MonadModuleBuilder m, MonadReader r m, ToHash r)
       => Text -> Type -> m Type
mkType typeName ty = do
  h <- asks getHash
  let typeNameWithHash = mkName $ T.unpack $ typeName <> "_" <> unHash h
  typedef typeNameWithHash (Just ty)

sizeOfType :: (Name, Type) -> ModuleBuilderT IO Word64
sizeOfType (n, ty) = do
  liftIO $ withHostTargetMachine Rel.PIC CM.Default CG.None $ \tm -> do
    dl <- getTargetMachineDataLayout tm
    Context.withContext $ flip runEncodeAST $ do
      createType n ty
      ty' <- encodeM ty
      liftIO $ DL.withFFIDataLayout dl $ flip DL.getTypeAllocSize ty'
  where
    createType :: Name -> Type -> EncodeAST ()
    createType n ty = do
      (t', n') <- createNamedType n
      defineType n n' t'
      setNamedType t' ty

-- NOTE: Orphan instance, but should give no conflicts.
instance MFunctor ModuleBuilderT where
  hoist nat = ModuleBuilderT . hoist nat . unModuleBuilderT

-- NOTE: Orphan instance, but should give no conflicts.
instance MFunctor IRBuilderT where
  hoist nat = IRBuilderT . hoist nat . unIRBuilderT

