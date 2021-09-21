{-# LANGUAGE RecursiveDo, FlexibleContexts, ScopedTypeVariables #-}

module Eclair.Data.BTree
  ( Meta(..)
  , Architecture(..)
  , SearchIndex
  , SearchType(..)
  , codegen
  ) where

import Protolude hiding ( Type, Meta )
import Protolude.Unsafe ( unsafeFromJust )
import qualified Data.Map as Map
import qualified Data.Set as Set
import LLVM.Pretty
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Name as Name
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Type
import LLVM.AST.Operand ( Operand(..) )
import LLVM.AST.DataLayout
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import qualified LLVM.Internal.FFI.DataLayout as DL
import qualified LLVM.Internal.DataLayout as DL
import qualified LLVM.Internal.Coding as Encode
import qualified LLVM.Internal.EncodeAST as Encode
import qualified LLVM.Internal.Type
import qualified LLVM.Internal.Context as Context
import qualified LLVM.Internal.FFI.PtrHierarchy as T
import Control.Arrow ((&&&))
import Data.Functor.Foldable


codegen :: Meta -> ModuleBuilder ()
codegen meta = do
  tys <- generateTypes meta
  runReaderT generateFunctions $ CGState meta tys

generateTypes :: Meta -> ModuleBuilder Types
generateTypes meta = mdo
  columnTy <- mkType "column_t" i32
  valueTy <- mkType "value_t" $ ArrayType (fromIntegral $ numColumns meta) columnTy
  positionTy <- mkType "position_t" i16
  nodeSizeTy <- mkType "node_size_t" i16  -- Note: used to be size_t/i64
  nodeTypeTy <- mkType "node_type_t" i8
  nodeDataTy <- mkType "node_data_t" $
    struct [ ptr nodeTy  -- parent
           , positionTy  -- position_in_parent
           , nodeSizeTy  -- num_elements
           , nodeTypeTy  -- node type
           ]
  nodeTy <- mkType "node_t" $
    struct [ nodeDataTy                 -- meta
           , ArrayType numKeys valueTy  -- values
           ]
  leafNodeTy <- mkType "leaf_node_t" nodeTy
  innerNodeTy <- mkType "inner_node_t" $
    struct [ nodeTy                                -- base
           , ArrayType (numKeys + 1) (ptr nodeTy)  -- children
           ]
  btreeIteratorTy <- mkType "btree_iterator_t" $
    struct [ ptr nodeTy  -- current
           , positionTy  -- value pos
           ]
  btreeTy <- mkType "btree_t" $
    struct [ ptr nodeTy  -- root
           , ptr nodeTy  -- first
           ]
  pure $ Types
    { btreeTy = btreeTy
    , iteratorTy = btreeIteratorTy
    , leafNodeTy = leafNodeTy
    , innerNodeTy = innerNodeTy
    , valueTy = valueTy
    , columnTy = columnTy
    }
  where
    mkType name ty = typedef name (Just ty)
    struct = StructureType False
    numKeys = fromIntegral $ max 3 desiredNumberOfKeys
      where
        blockByteSize = blockSize meta
        nodeMetaSize = if arch meta == X86 then 12 else 16
        valueByteSize = numColumns meta * 4
        valuesByteSize =
          if blockByteSize > nodeMetaSize
          then blockByteSize - nodeMetaSize
          else 0
        desiredNumberOfKeys = valuesByteSize `div` valueByteSize

generateFunctions :: ModuleCodegen ()
generateFunctions = do
  mkCompare

mkCompare :: ModuleCodegen ()
mkCompare = do
  (tys, meta) <- asks (types &&& meta)
  let column = columnTy tys
      value = valueTy tys
  compare <- function "compare" [(column, "lhs"), (column, "rhs")] i8 $ \[lhs, rhs] -> mdo
    result1 <- icmp IP.ULT lhs rhs
    condBr result1 lt gtOrEq
    lt <- block `named` "lt"
    ret $ int8 (-1)
    gtOrEq <- block `named` "gt_or_eq"
    result2 <- icmp IP.UGT lhs rhs
    ret =<< select result2 (int8 1) (int8 0)

  function "compare_values" [(ptr value, "lhs"), (ptr value, "rhs")] i8 $ \[lhs, rhs] -> mdo
    let columns = map fromIntegral $ Set.toList $ index meta
    results <- flip execStateT mempty $ flip (zygo endCheck) columns $ \case
      Nil -> pure ()
      Cons col (atEnd, asm) -> do
        blk <- block `named` "comparison"
        let indices = [int32 0, int32 col]
        lhsPtr <- gep lhs indices
        rhsPtr <- gep rhs indices
        lhsValue <- load lhsPtr 0
        rhsValue <- load rhsPtr 0
        compareResult <- call compare [(lhsValue, []), (rhsValue, [])]
        modify $ Map.insert compareResult blk
        case atEnd of
          End -> br end
          Continue -> mdo
            isEqual <- icmp IP.EQ compareResult (int8 0)
            condBr isEqual continue end
            asm
            continue <- currentBlock
            pure ()
    end <- block `named` "end"
    ret =<< phi (Map.toList results)

  pure ()
  where
    endCheck = \case
      Nil -> End
      _ -> Continue

data AtEnd = Continue | End
  deriving Eq

  {-
  function "compare_values" [(ptr value, "lhs"), (ptr value, "rhs")] i8 $ \[lhs, rhs] -> mdo
    let columns = map fromIntegral $ Set.toList $ index meta
    results <- flip execStateT mempty $ flip (zygo endCheck) columns $ \case
      Cons col (atEnd, asm) -> mdo
        blk <- block `named` "comparison"
        let indices = [int32 0, int32 col]
        lhsPtr <- gep lhs indices
        rhsPtr <- gep rhs indices
        lhsValue <- load lhsPtr 0
        rhsValue <- load rhsPtr 0
        compareResult <- call compare [(lhsValue, []), (rhsValue, [])]
        modify $ Map.insert compareResult blk
        case atEnd of
          End -> br end
          Continue -> mdo
            isEqual <- icmp IP.EQ compareResult (int8 0)
            condBr isEqual continue end
            continue <- asm
            pure ()

        pure blk
      Nil -> panic "Unreachable!"
    end <- block `named` "end"
    ret =<< phi (Map.toList results)

  pure ()
  where
    endCheck = \case
      Nil -> End
      _ -> Continue

data AtEnd = Continue | End
-}

  {-
    f lhs rhs compare end = \case
      Cons col (remainingBlocks, asm) -> mdo
        blk <- block `named` "comparison"
        let indices = [int32 0, int32 col]
        lhsPtr <- gep lhs indices
        rhsPtr <- gep rhs indices
        lhsValue <- load lhsPtr 0
        rhsValue <- load rhsPtr 0
        compareResult <- call compare [(lhsValue, []), (rhsValue, [])]
        modify $ Map.insert compareResult blk
        if remainingBlocks > 0
          then mdo
            isEqual <- icmp IP.EQ compareResult (int8 0)
            condBr isEqual continue end
            continue <- asm
            pure ()
          else br end

        pure blk
      Nil -> pure (panic "Ruh roh")
-}



  {-
  func "compare_values" [(ptr value, "lhs"), (ptr value, "rhs")] i8 $ \[lhs, rhs] -> mdo
    let columns = map fromIntegral $ Set.toList $ index meta
    (compareResult, _) <- foldrM (f lhs rhs compare) (int32 0, end) $ reverse columns
    end <- block `named` "end"
    ret compareResult

  pure ()
  where
    f lhs rhs compare col (_, labelUnmatched) = mdo
      let indices = [int32 0, int32 col]
      lhsPtr <- gep lhs indices
      rhsPtr <- gep rhs indices
      lhsValue <- load lhsPtr 0
      rhsValue <- load rhsPtr 0
      compareResult <- call compare [(lhsValue, []), (rhsValue, [])]
      isEqual <- icmp IP.EQ compareResult (int8 0)
      condBr isEqual continue labelUnmatched
      continue <- block `named` "continue"
      pure (compareResult, continue)
-}

{-
  func "compare_values" [(ptr value, "lhs"), (ptr value, "rhs")] i8 $ \[lhs, rhs] -> mdo
    let columns = map fromIntegral $ Set.toList $ index meta
    (compareResult, _) <- ifLadder lhs rhs compare (int32 0, end) columns
    end <- block `named` "end"
    ret compareResult


  pure ()
  where
    ifLadder lhs rhs compare result@(compareResult, labelUnmatched) = \case
      [] -> pure result
      [col] -> mdo
        let indices = [int32 0, int32 col]
        lhsValue <- gep lhs indices
        rhsValue <- gep rhs indices
        compareResult' <- call compare [(lhsValue, []), (rhsValue, [])]
        ret compareResult'
        continue <- currentBlock
        pure (compareResult', continue)

      col:cols -> mdo
        let indices = [int32 0, int32 col]
        lhsValue <- gep lhs indices
        rhsValue <- gep rhs indices
        compareResult' <- call compare [(lhsValue, []), (rhsValue, [])]
        isEqual <- icmp IP.EQ compareResult' (int8 0)
        condBr isEqual continue labelUnmatched
        continue <- block `named` "continue"
        (_, labelUnmatched') <- ifLadder lhs rhs compare (compareResult', continue) cols
        pure (compareResult', labelUnmatched')
-}


data Types
  = Types
  { btreeTy :: Type
  , iteratorTy :: Type
  , leafNodeTy :: Type
  , innerNodeTy :: Type
  , valueTy :: Type
  , columnTy :: Type
  }

data CGState = CGState { meta :: Meta, types :: Types }

type ModuleCodegen = ReaderT CGState ModuleBuilder

type IRCodegen = IRBuilderT ModuleCodegen


-- TODO: move as much as possible to "common llvm helpers file"
-- Some type synonyms for readability

type Variable = ParameterName

-- During codegen we need to keep track of a mapping from variable names to operands (%0, %1, ...) in IR
type CodeGenState = Map Variable Operand

type CodegenM = ReaderT CodeGenState (IRBuilderT ModuleBuilder)

lookupVar :: Variable -> CodegenM Operand
lookupVar v = asks $ unsafeFromJust . Map.lookup v

-- Btree specific code:

data Meta
  = Meta
  { arch :: Architecture     -- 32- or 64-bit architecture
  , numColumns :: Int        -- Amount of columns each node has
  , blockSize :: Int         -- Number of bytes per btree node
  , index :: SearchIndex     -- Which columns are used to index values
  , searchType :: SearchType -- Search strategy used in a single node
  }

data Architecture
  = X86
  | X64
  deriving Eq

type Column = Int

type SearchIndex = Set Column

data SearchType = Linear | Binary

