{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.LLVM.BTree.Compare
  ( mkCompare
  ) where

import Eclair.LLVM.BTree.Types
import qualified Data.Map as Map


mkCompare :: ModuleCodegen Operand
mkCompare = do
  settings <- getParams
  tys <- asks types
  let column' = columnTy tys
      value = valueTy tys
  compare' <- function "eclair_btree_value_compare" [(column', "lhs"), (column', "rhs")] i8 $ \[lhs, rhs] -> mdo
    result1 <- lhs `ult` rhs
    if' result1 $
      ret $ int8 (-1)
    result2 <- lhs `ugt` rhs
    ret =<< select result2 (int8 1) (int8 0)

  function "eclair_btree_value_compare_values" [(ptr value, "lhs"), (ptr value, "rhs")] i8 $ \[lhs, rhs] -> mdo
    let columns = map fromIntegral $ index settings
    results <- flip execStateT mempty $ flip (zygo endCheck) columns $ \case
      Nil -> pass
      Cons col (atEnd, asm) -> do
        blk <- blockNamed "comparison"
        let indices = [int32 0, int32 col]
        lhsPtr <- gep lhs indices
        rhsPtr <- gep rhs indices
        lhsValue <- load lhsPtr 0
        rhsValue <- load rhsPtr 0
        compareResult <- call compare' [lhsValue, rhsValue]
        modify $ Map.insert compareResult blk
        case atEnd of
          End -> br end
          Continue -> mdo
            isEqual <- compareResult `eq` int8 0
            condBr isEqual continue end
            asm
            continue <- currentBlock
            pass
    end <- blockNamed "end"
    ret =<< phi (Map.toList results)
  where
    endCheck = \case
      Nil -> End
      _ -> Continue

data ControlFlow
  = Continue
  | End
