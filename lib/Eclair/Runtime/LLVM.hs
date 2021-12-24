{-# LANGUAGE RoleAnnotations, RecursiveDo, PolyKinds #-}

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


int16 :: Integer -> Operand
int16 = ConstantOperand . Constant.Int 16

nullPtr :: Type -> Operand
nullPtr = ConstantOperand . Constant.Null . ptr

eq, ne, sge, sgt, sle, slt, uge, ugt, ule, ult
  :: MonadIRBuilder m => Operand -> Operand -> m Operand
eq = icmp IP.EQ
ne = icmp IP.NE
sge = icmp IP.SGE
sgt = icmp IP.SGT
sle = icmp IP.SLE
slt = icmp IP.SLT
uge = icmp IP.UGE
ugt = icmp IP.UGT
ule = icmp IP.ULE
ult = icmp IP.ULT

if' :: (MonadIRBuilder m, MonadFix m)
    => Operand -> m a -> m ()
if' condition asm = mdo
  condBr condition ifBlock end
  ifBlock <- block `named` "if"
  asm
  br end
  end <- block `named` "end_if"
  pure ()

-- Note: this loops forever, only way to exit is if the inner block of ASM
-- Jumps to a label outside the loop
loop :: (MonadIRBuilder m, MonadFix m) => m a -> m ()
loop asm = mdo
  br begin
  begin <- block `named` "loop"
  asm
  br begin

whileLoop :: (MonadIRBuilder m, MonadFix m)
          => m Operand -> m a -> m ()
whileLoop condition asm = mdo
  br begin
  begin <- block `named` "while_begin"
  result <- condition
  condBr result body end
  body <- block `named` "while_body"
  asm
  br begin
  end <- block `named` "while_end"
  pure ()

forLoop :: (MonadModuleBuilder m, MonadIRBuilder m, MonadFix m)
        => Operand
        -> (Operand -> m Operand)
        -> (Operand -> m Operand)
        -> (Operand -> m a)
        -> m ()
forLoop beginValue condition post asm = mdo
  start <- currentBlock
  br begin
  begin <- block `named` "for_begin"
  loopValue <- phi [(beginValue, start), (updatedValue, bodyEnd)]
  result <- condition loopValue
  condBr result bodyStart end
  bodyStart <- block `named` "for_body"
  asm loopValue
  updatedValue <- post loopValue
  bodyEnd <- currentBlock
  br begin
  end <- block `named` "for_end"
  pure ()

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

newtype Path (a :: k) (b :: k)
  = Path (NonEmpty Operand)
type role Path nominal nominal

(->>) :: Path a b -> Path b c -> Path a c
Path a2b ->> Path b2c =
  let b2c' = if NE.head b2c == int32 0
               then NE.tail b2c
               else NE.toList b2c
   in Path $ NE.head a2b :| (NE.tail a2b ++ b2c')

mkPath :: [Operand] -> Path a b
mkPath path = Path (int32 0 :| path)

addr :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Path a b -> Operand -> m Operand
addr path p = gep p (pathToIndices path)
  where
    pathToIndices :: Path a b -> [Operand]
    pathToIndices (Path indices) =
      NE.toList indices

deref :: (MonadModuleBuilder m, MonadIRBuilder m)
      => Path a b -> Operand -> m Operand
deref path p = do
  addr <- addr path p
  load addr 0

assign :: (MonadModuleBuilder m, MonadIRBuilder m)
       => Path a b -> Operand -> Operand -> m ()
assign path p value = do
  dstAddr <- addr path p
  store dstAddr 0 value

update :: (MonadModuleBuilder m, MonadIRBuilder m)
       => Path a b
       -> Operand
       -> (Operand -> m Operand)
       -> m ()
update path p f = do
  dstAddr <- addr path p
  store dstAddr 0 =<< f =<< load dstAddr 0

increment :: (MonadModuleBuilder m, MonadIRBuilder m)
          => (Integer -> Operand) -> Path a b -> Operand -> m ()
increment ty path p = update path p (add (ty 1))

copy :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Path a b -> Operand -> Operand -> m ()
copy path src dst = do
  value <- deref path src
  assign path dst value

swap :: (MonadModuleBuilder m, MonadIRBuilder m)
     => Path a b -> Operand -> Operand -> m ()
swap path lhs rhs = do
  tmp <- deref path lhs
  copy path rhs lhs
  assign path rhs tmp

allocate :: MonadIRBuilder m => Type -> Operand -> m Operand
allocate ty beginValue = do
  value <- alloca ty (Just (int32 1)) 0
  store value 0 beginValue
  pure value

-- NOTE: only works for unsigned integers!
minimum :: (MonadModuleBuilder m, MonadIRBuilder m)
        => Operand -> Operand -> m Operand
minimum a b = do
  isLessThan <- a `ult` b
  select isLessThan a b

pointerDiff :: (MonadModuleBuilder m, MonadIRBuilder m)
            => Type -> Operand -> Operand -> m Operand
pointerDiff ty a b = do
  a' <- ptrtoint a i64
  b' <- ptrtoint b i64
  result <- sub a' b'
  trunc result ty

-- NOTE: assumes input is of type i1
not :: (MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> m Operand
not bool = select bool (bit 0) (bit 1)

-- NOTE: Orphan instance, but should give no conflicts.
instance MFunctor ModuleBuilderT where
  hoist nat = ModuleBuilderT . hoist nat . unModuleBuilderT

-- NOTE: Orphan instance, but should give no conflicts.
instance MFunctor IRBuilderT where
  hoist nat = IRBuilderT . hoist nat . unIRBuilderT

