{-# LANGUAGE RecursiveDo, FlexibleContexts, ScopedTypeVariables #-}

module Eclair.Data.BTree
  ( Meta(..)
  , Architecture(..)
  , SearchIndex
  , SearchType(..)
  , codegen
  ) where

import Protolude hiding ( Type, Meta, void, bit )
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
  exts <- mkExternals
  runReaderT generateFunctions $ CGState meta tys exts

mkExternals :: ModuleBuilder Externals
mkExternals = do
  malloc <- extern "malloc" [i32] (ptr i8)
  free <- extern "free" [ptr i8] void
  memset <- extern "llvm.memset.p0i8.i64" [ptr i8, i8, i64, i1] void
  pure $ Externals malloc free memset

generateTypes :: Meta -> ModuleBuilder Types
generateTypes meta = mdo
  columnTy <- mkType "column_t" i32
  valueTy <- mkType "value_t" $ ArrayType (fromIntegral $ numColumns meta) columnTy
  positionTy <- mkType "position_t" i16
  nodeSizeTy <- mkType "node_size_t" i16  -- Note: used to be size_t/i64
  nodeTypeTy <- mkType "node_type_t" i1
  nodeDataTy <- mkType "node_data_t" $
    struct [ ptr nodeTy  -- parent
           , positionTy  -- position_in_parent
           , nodeSizeTy  -- num_elements
           , nodeTypeTy  -- node type
           ]
  nodeTy <- mkType "node_t" $
    struct [ nodeDataTy                 -- meta
           , ArrayType (numKeys meta) valueTy  -- values
           ]
  leafNodeTy <- mkType "leaf_node_t" nodeTy
  innerNodeTy <- mkType "inner_node_t" $
    struct [ nodeTy                                -- base
           , ArrayType (numKeys meta + 1) (ptr nodeTy)  -- children
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
    , nodeTypeTy = nodeTypeTy
    , nodeTy = nodeTy
    , leafNodeTy = leafNodeTy
    , innerNodeTy = innerNodeTy
    , valueTy = valueTy
    , columnTy = columnTy
    }
  where
    mkType name ty = typedef name (Just ty)
    struct = StructureType False

numKeys :: Meta -> Word64
numKeys meta = fromIntegral $ max 3 desiredNumberOfKeys
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
  mkNodeNew

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

data ControlFlow = Continue | End

mkNodeNew :: ModuleCodegen ()
mkNodeNew = mdo
  md <- asks meta
  -- TODO refactor
  (nodeType, (node, innerNode)) <- asks ((nodeTypeTy &&& nodeTy &&& innerNodeTy) . types)
  (malloc, memset) <- asks ((extMalloc &&& extMemset) . externals)
  function "node_new" [(nodeType, "type")] (ptr node) $ \[ty] -> mdo
    structSize <- select ty leafNodeSize innerNodeSize
    memory <- call malloc [(structSize, [])]
    n <- memory `bitcast` ptr node

    metaPtr <- gep n [int32 0, int32 0]
    parentPtr <- gep metaPtr [int32 0, int32 0]
    posInParentPtr <- gep metaPtr [int32 0, int32 1]
    numElementsPtr <- gep metaPtr [int32 0, int32 2]
    nodeTypePtr <- gep metaPtr [int32 0, int32 3]
    store parentPtr 0 (nullPtr node)
    store posInParentPtr 0 (int16 0)
    store numElementsPtr 0 (int16 0)
    store nodeTypePtr 0 ty

    valuesPtr <- gep n [int32 0, int32 1]
    valuesPtr' <- valuesPtr `bitcast` ptr i8
    let valuesByteCount = toInteger (numKeys md) * valueSize
    call memset [(valuesPtr', []), (int8 0, []), (int64 valuesByteCount, []), (bit 0, [])]

    isInner <- icmp IP.EQ ty innerNodeTypeVal
    condBr isInner initInner end

    initInner <- block `named` "init_inner"
    inner <- n `bitcast` ptr innerNode
    childrenPtr <- gep inner [int32 0, int32 1]
    childrenPtr' <- childrenPtr `bitcast` ptr i8
    let childrenByteCount = toInteger (numKeys md + 1) * nodePtrSize md
    call memset [(childrenPtr', []), (int8 0, []), (int64 childrenByteCount, []), (bit 0, [])]
    br end

    end <- block `named` "end"
    ret n

  pure ()
  where
    nodePtrSize md = case arch md of X86 -> 4; X64 -> 8
    leafNodeSize = int32 1000 -- TODO
    innerNodeSize = int32 1000  -- TODO
    valueSize = 1000  -- TODO
    -- TODO: refactor next 2 lines outside of function
    leafNodeTypeVal = bit 0
    innerNodeTypeVal = bit 1

nullPtr :: Type -> Operand
nullPtr = ConstantOperand . Constant.Null . ptr


data Types
  = Types
  { btreeTy :: Type
  , iteratorTy :: Type
  , nodeTypeTy :: Type
  , nodeTy :: Type
  , leafNodeTy :: Type
  , innerNodeTy :: Type
  , valueTy :: Type
  , columnTy :: Type
  }

data Externals
  = Externals
  { extMalloc :: Operand
  , extFree :: Operand
  , extMemset :: Operand
  }

data CGState
  = CGState
  { meta :: Meta
  , types :: Types
  , externals :: Externals
  }

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

int16 :: Integer -> Operand
int16 = ConstantOperand . Constant.Int 16

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

