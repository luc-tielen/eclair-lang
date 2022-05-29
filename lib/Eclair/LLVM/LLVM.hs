{-# LANGUAGE RoleAnnotations, PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Eclair.LLVM.LLVM
  ( module Eclair.LLVM.LLVM
  ) where

import qualified Data.Text as T
import qualified Data.Map as M
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
       => Text -> Flag Packed -> [Type] -> m Type
mkType typeName packed tys = do
  s <- asks (show . getSuffix)
  let typeNameWithHash = Name $ typeName <> "_" <> s
  typedef typeNameWithHash packed tys

llvmSizeOf :: ForeignPtr LibLLVM.Context -> Ptr LibLLVM.TargetData -> Type -> ModuleBuilderT IO Word64
llvmSizeOf ctx td ty = liftIO $ do
  ty' <- encodeType ctx ty
  LibLLVM.sizeOfType td ty'

withLLVMTypeInfo :: (ForeignPtr LibLLVM.Context -> Ptr LibLLVM.TargetData -> ModuleBuilderT IO a)
                 -> ModuleBuilderT IO a
withLLVMTypeInfo f = do
  (ctx, td) <- liftIO $ do
    ctx <- LibLLVM.mkContext
    llvmMod <- LibLLVM.mkModule ctx "<internal_use_only>"
    td <- LibLLVM.getTargetData llvmMod
    pure (ctx, td)

  -- First, we forward declare all struct types known up to this point,
  typedefs <- getTypedefs
  structTys <- liftIO $ M.traverseWithKey (forwardDeclareStruct ctx) typedefs

  -- Then we serialize all types (including structs, with their bodies),
  liftIO $ M.traverseWithKey (serialize ctx) structTys
  -- Finally, we can call the function with all type info available in LLVM.
  f ctx td
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
