{-# LANGUAGE RoleAnnotations, PolyKinds #-}

module Eclair.LLVM.Codegen
  ( module Eclair.LLVM.Codegen
  , module Eclair.LLVM.Template
  , module Eclair.LLVM.Config
  , module LLVM.Codegen
  ) where

import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.Morph
import Foreign.ForeignPtr
import Foreign.Ptr hiding (nullPtr)
import Eclair.LLVM.Template
import LLVM.Codegen hiding (function, typedef, typeOf)
import qualified LLVM.C.API as LibLLVM
import Eclair.LLVM.Config


llvmSizeOf :: (MonadModuleBuilder m, MonadIO m)
           => ForeignPtr LibLLVM.Context -> Ptr LibLLVM.TargetData -> Type -> m Word64
llvmSizeOf ctx td ty = liftIO $ do
  ty' <- encodeType ctx ty
  LibLLVM.sizeOfType td ty'

withLLVMTypeInfo :: (MonadModuleBuilder m, MonadIO m)
                 => ForeignPtr LibLLVM.Context -> m a -> m a
withLLVMTypeInfo ctx m = do
  -- First, we forward declare all struct types known up to this point,
  typedefs <- getTypedefs
  structTys <- liftIO $ M.traverseWithKey (forwardDeclareStruct ctx) typedefs

  -- Then we serialize all types (including structs, with their bodies),
  liftIO $ M.traverseWithKey (serialize ctx) structTys
  -- Finally, we can call the function with all type info available in LLVM.
  m
  where
    forwardDeclareStruct ctx name structTy =
      (,structTy) <$> LibLLVM.mkOpaqueStructType ctx name

    serialize :: ForeignPtr LibLLVM.Context -> Name -> (Ptr LibLLVM.Type, Type) -> IO ()
    serialize ctx name (llvmTy, ty) = case ty of
      StructureType packed tys -> do
        tys' <- traverse (encodeType ctx) tys
        LibLLVM.setNamedStructBody llvmTy tys' packed
      ty ->
        panic $ "Unexpected typedef: only structs are allowed, but got: " <> show ty

-- NOTE: this only works if all the named structs are known beforehand (a.k.a. forward declared)!
encodeType :: ForeignPtr LibLLVM.Context -> Type -> IO (Ptr LibLLVM.Type)
encodeType ctx = go
  where
    go = \case
      VoidType ->
        LibLLVM.mkVoidType ctx
      IntType bits ->
        LibLLVM.mkIntType ctx bits
      PointerType ty ->
        LibLLVM.mkPointerType =<< go ty
      StructureType packed tys -> do
        tys' <- traverse go tys
        LibLLVM.mkAnonStructType ctx tys' packed
      ArrayType count ty -> do
        ty' <- go ty
        LibLLVM.mkArrayType ty' count
      FunctionType retTy argTys -> do
        retTy' <- go retTy
        argTys' <- traverse go argTys
        LibLLVM.mkFunctionType retTy' argTys'
      NamedTypeReference name ->
        LibLLVM.getTypeByName ctx name
