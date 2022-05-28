{-# LANGUAGE RoleAnnotations, PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Eclair.LLVM.LLVM
  ( module Eclair.LLVM.LLVM
  ) where

import qualified Data.Text as T
import Control.Monad.Morph
import Foreign.ForeignPtr
import Foreign.Ptr
import LLVM.Codegen
import qualified LLVM.C.API as LibLLVM
import Eclair.LLVM.Runtime

def :: (MonadModuleBuilder m, MonadReader r m, HasSuffix r)
    => Text
    -> [(Type, ParameterName)]
    -> Type
    -> ([Operand] -> IRBuilderT m ())
    -> m Operand
def funcName args retTy body = do
  s <- asks (show . getSuffix)
  let funcNameWithHash = Name $ funcName <> "_" <> s
  function funcNameWithHash args retTy body

mkType :: (MonadModuleBuilder m, MonadReader r m, HasSuffix r)
       => Text -> Type -> m Type
mkType typeName ty = do
  s <- asks (show . getSuffix)
  let typeNameWithHash = Name $ typeName <> "_" <> s
  typedef typeNameWithHash ty

sizeOfType :: Type -> ModuleBuilderT IO Word64
sizeOfType ty = liftIO $ do
  ctx <- LibLLVM.mkContext
  llvmMod <- LibLLVM.mkModule ctx "<internal_use_only>"
  td <- LibLLVM.getTargetData llvmMod
  ty' <- mkLLVMType ctx ty
  LibLLVM.sizeOfType td ty'

-- TODO export as a helper function in llvm-codegen
mkLLVMType :: ForeignPtr LibLLVM.Context -> Type -> IO (Ptr LibLLVM.Type)
mkLLVMType ctx = \case
  VoidType ->
    LibLLVM.mkVoidType ctx
  IntType bits ->
    LibLLVM.mkIntType ctx bits
  PointerType ty ->
    LibLLVM.mkPointerType =<< mkLLVMType ctx ty
  StructureType packed tys -> do
    tys' <- traverse (mkLLVMType ctx) tys
    LibLLVM.mkStructType ctx tys' packed
  ArrayType count ty -> do
    ty' <- mkLLVMType ctx ty
    LibLLVM.mkArrayType ty' count
  FunctionType retTy argTys -> do
    retTy' <- mkLLVMType ctx retTy
    argTys' <- traverse (mkLLVMType ctx) argTys
    LibLLVM.mkFunctionType retTy' argTys'
  NamedTypeReference name ->
    LibLLVM.getTypeByName ctx name

