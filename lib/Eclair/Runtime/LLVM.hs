{-# LANGUAGE RecursiveDo #-}

module Eclair.Runtime.LLVM
  ( module Eclair.Runtime.LLVM
  ) where

import Protolude hiding ( Type )
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
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
