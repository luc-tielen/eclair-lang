{-# LANGUAGE RoleAnnotations, RecursiveDo, PolyKinds #-}

module Eclair.Runtime.LLVM
  ( module Eclair.Runtime.LLVM
  ) where

import Protolude hiding ( Type, (.), bit )
import Control.Category
import qualified Data.List.NonEmpty as NE
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Constant as Constant
import LLVM.AST.Operand ( Operand(..) )
import LLVM.AST.Type


type ModuleCodegen r = ReaderT r ModuleBuilder

type IRCodegen r = IRBuilderT (ModuleCodegen r)

int16 :: Integer -> Operand
int16 = ConstantOperand . Constant.Int 16

nullPtr :: Type -> Operand
nullPtr = ConstantOperand . Constant.Null . ptr

eq, ne, sge, sgt, sle, slt, uge, ugt, ule, ult
  :: Operand -> Operand -> IRCodegen r Operand
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

if' :: Operand -> IRCodegen r a -> IRCodegen r ()
if' condition asm = mdo
  condBr condition ifBlock end
  ifBlock <- block `named` "if"
  asm
  br end
  end <- block `named` "end_if"
  pure ()

-- Note: this loops forever, only way to exit is if the inner block of ASM
-- Jumps to a label outside the loop
loop :: IRCodegen r a -> IRCodegen r ()
loop asm = mdo
  br begin
  begin <- block `named` "loop"
  asm
  br begin

whileLoop :: IRCodegen r Operand -> IRCodegen r a -> IRCodegen r ()
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

forLoop :: Operand
        -> (Operand -> IRCodegen r Operand)
        -> (Operand -> IRCodegen r Operand)
        -> (Operand -> IRCodegen r a)
        -> IRCodegen r ()
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

addr :: Path a b -> Operand -> IRCodegen r Operand
addr path p = gep p (pathToIndices path)
  where
    pathToIndices :: Path a b -> [Operand]
    pathToIndices (Path indices) =
      NE.toList indices

deref :: Path a b -> Operand -> IRCodegen r Operand
deref path p = do
  addr <- addr path p
  load addr 0

assign :: Path a b -> Operand -> Operand -> IRCodegen r ()
assign path p value = do
  dstAddr <- addr path p
  store dstAddr 0 value

update :: Path a b
       -> Operand
       -> (Operand -> IRCodegen r Operand)
       -> IRCodegen r ()
update path p f = do
  dstAddr <- addr path p
  store dstAddr 0 =<< f =<< load dstAddr 0

increment :: (Integer -> Operand) -> Path a b -> Operand -> IRCodegen r ()
increment ty path p = update path p (add (ty 1))

copy :: Path a b -> Operand -> Operand -> IRCodegen r ()
copy path src dst = do
  value <- deref path src
  assign path dst value

swap :: Path a b -> Operand -> Operand -> IRCodegen r ()
swap path lhs rhs = do
  tmp <- deref path lhs
  copy path rhs lhs
  assign path rhs tmp

allocate :: Type -> Operand -> IRCodegen r Operand
allocate ty beginValue = do
  value <- alloca ty (Just (int32 1)) 0
  store value 0 beginValue
  pure value

-- NOTE: only works for unsigned integers!
minimum :: Operand -> Operand -> IRCodegen r Operand
minimum a b = do
  isLessThan <- a `ult` b
  select isLessThan a b

pointerDiff :: Type -> Operand -> Operand -> IRCodegen r Operand
pointerDiff ty a b = do
  a' <- ptrtoint a i64
  b' <- ptrtoint b i64
  result <- sub a' b'
  trunc result ty

-- NOTE: assumes input is of type i1
not :: Operand -> IRCodegen r Operand
not bool = select bool (bit 0) (bit 1)

