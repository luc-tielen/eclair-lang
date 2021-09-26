{-# LANGUAGE RecursiveDo, PolyKinds #-}

module Eclair.Runtime.LLVM
  ( module Eclair.Runtime.LLVM
  ) where

import Protolude hiding ( Type, (.) )
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

instance Category Path where
  id = Path (pure $ int32 0)
  Path b2c . Path a2b =
    Path $ NE.head a2b :| (NE.tail a2b ++ NE.tail b2c)

mkPath :: [Operand] -> Path a b
mkPath path = Path (int32 0 :| path)

pathToIndices :: Path a b -> [Operand]
pathToIndices (Path indices) =
  NE.toList indices

addr :: Path a b -> Operand -> IRCodegen r Operand
addr path p = gep p (pathToIndices path)

deref :: Path a b -> Operand -> IRCodegen r Operand
deref path p = do
  addr <- addr path p
  load addr 0

assign :: Path a b -> Operand -> Operand -> IRCodegen r ()
assign path p value = do
  dstAddr <- addr path p
  store dstAddr 0 value

copy :: Path a b -> Operand -> Operand -> IRCodegen r ()
copy path src dst = do
  value <- deref path src
  assign path dst value
