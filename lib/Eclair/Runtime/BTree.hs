{-# LANGUAGE RecursiveDo, FlexibleContexts, ScopedTypeVariables #-}

module Eclair.Runtime.BTree
  ( Meta(..)
  , Architecture(..)
  , SearchIndex
  , SearchType(..)
  , codegen
  ) where

import Protolude hiding ( Type, Meta, void, bit, typeOf, minimum )
import Control.Arrow ((&&&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Functor.Foldable
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Type
import LLVM.AST.Operand ( Operand(..) )
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import Eclair.Runtime.LLVM hiding (IRCodegen, ModuleCodegen)
import qualified Eclair.Runtime.LLVM as LLVM


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
    , nodeSizeTy = nodeSizeTy
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

generateFunctions :: ModuleCodegen ()
generateFunctions = do
  mkCompare
  nodeNew <- mkNodeNew
  mkNodeDelete
  mkNodeClone nodeNew
  mkNodeDepth
  mkNodeCount
  mkNodeCountEntries
  mkNodeIsEmpty
  mkNodeIsFull
  mkNodeSplitPoint
  pure ()

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

mkNodeNew :: ModuleCodegen Operand
mkNodeNew = mdo
  md <- asks meta
  nodeType <- typeOf NodeType
  node <- typeOf Node
  innerNode <- typeOf InnerNode

  leafNodeSize <- int32 . toInteger <$> sizeOf Node
  innerNodeSize <- int32 . toInteger <$> sizeOf InnerNode
  valueSize <- sizeOf Value

  malloc <- asks (extMalloc . externals)

  function "node_new" [(nodeType, "type")] (ptr node) $ \[ty] -> mdo
    structSize <- select ty leafNodeSize innerNodeSize
    memory <- call malloc [(structSize, [])]
    n <- memory `bitcast` ptr node

    assign (metaOf ->> parentOf) n (nullPtr node)
    assign (metaOf ->> posInParentOf) n (int16 0)
    assign (metaOf ->> numElemsOf) n (int16 0)
    assign (metaOf ->> nodeTypeOf) n ty

    let valuesByteCount = numKeys md * valueSize
    valuesPtr <- addr valuesOf n
    memset valuesPtr 0 valuesByteCount

    isInner <- ty `eq` innerNodeTypeVal
    condBr isInner initInner end

    initInner <- block `named` "init_inner"
    inner <- n `bitcast` ptr innerNode
    let childrenByteCount = (numKeys md + 1) * ptrSize md
    childrenPtr <- addr childrenOf inner
    memset childrenPtr 0 childrenByteCount
    br end

    end <- block `named` "end"
    ret n

mkNodeDelete :: ModuleCodegen Operand
mkNodeDelete = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  free <- asks (extFree . externals)

  nodeDelete <- function "node_delete" [(ptr node, "node")] void $ \[n] -> mdo
    nodeTy <- deref (metaOf ->> nodeTypeOf) n
    condBr nodeTy deleteInner end

    deleteInner <- block `named` "delete_inner"
    inner <- n `bitcast` ptr innerNode

    numElements <- deref (metaOf ->> numElemsOf) n
    forLoop (int16 0) (`ule` numElements) (add (int16 1)) $ \i -> mdo
      child <- deref (childAt i) inner
      isNotNull <- child `ne` nullPtr node
      if' isNotNull $
        call nodeDelete [(child, [])]

    br end

    end <- block `named` "end"
    memory <- n `bitcast` ptr i8
    call free [(memory, [])]
    pure ()

  pure nodeDelete

mkNodeClone :: Operand -> ModuleCodegen ()
mkNodeClone nodeNew = mdo
  node <- typeOf Node

  nodeClone <- function "node_clone" [(ptr node, "node")] (ptr node) $ \[n] -> mdo
    ty <- deref (metaOf ->> nodeTypeOf) n
    newNode <- call nodeNew [(ty, [])]
    condBr ty cloneInner cloneLeaf

    cloneInner <- block `named` "clone_inner"
    copyNode n newNode
    copyChildren nodeClone n newNode
    br end

    cloneLeaf <- block `named` "clone_leaf"
    copyNode n newNode
    br end

    end <- block `named` "end"
    ret newNode
  pure ()
  where
    copyNode n newNode = mdo
      -- NOTE: original impl did copied everything except for parent pointer
      nMeta <- copy metaOf n newNode

      numElements <- deref (metaOf ->> numElemsOf) n
      forLoop (int16 0) (`ult` numElements) (add (int16 1)) $ \i -> mdo
        copy (valueAt i) n newNode

    copyChildren nodeClone n newNode = mdo
      innerNode <- typeOf InnerNode
      innerN <- n `bitcast` ptr innerNode
      newInnerN <- newNode `bitcast` ptr innerNode

      numElements <- deref (metaOf ->> numElemsOf) n
      forLoop (int16 0) (`ule` numElements) (add (int16 1)) $ \i -> mdo
        child <- deref (childAt i) innerN
        clonedChild <- call nodeClone [(child, [])]
        assign (metaOf ->> parentOf) clonedChild newNode
        assign (childAt i) newInnerN clonedChild

mkNodeDepth :: ModuleCodegen ()
mkNodeDepth = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  nodeSize <- typeOf NodeSize

  nodeDepth <- function "node_depth" [(ptr node, "node")] nodeSize $ \[n] -> mdo
    ty <- deref (metaOf ->> nodeTypeOf) n
    condBr ty depthInner depthLeaf

    depthInner <- block `named` "depth_inner"
    inner <- n `bitcast` ptr innerNode
    child <- deref (childAt (int16 0)) inner
    depthChild <- call nodeDepth [(child, [])]
    depth <- add (int16 1) depthChild
    ret depth

    depthLeaf <- block `named` "depth_leaf"
    ret (int16 1)

  pure ()

mkNodeCount :: ModuleCodegen ()
mkNodeCount = mdo
  node <- typeOf Node

  countNodes <- function "node_count" [(ptr node, "node")] i64 $ \[n] -> mdo
    ty <- deref (metaOf ->> nodeTypeOf) n
    condBr ty countInner countLeaf

    countInner <- block `named` "count_inner"
    count <- loopChildren n i64 (int64 1) $ \nodeCount child -> mdo
      childNodeCount <- call countNodes [(child, [])]
      add nodeCount childNodeCount
    ret count

    countLeaf <- block `named` "count_leaf"
    ret (int64 1)

  pure ()

mkNodeCountEntries :: ModuleCodegen ()
mkNodeCountEntries = mdo
  node <- typeOf Node

  countEntries <- function "node_count_entries" [(ptr node, "node")] i64 $ \[n] -> mdo
    numElements <- deref (metaOf ->> numElemsOf) n
    ty <- deref (metaOf ->> nodeTypeOf) n
    condBr ty countInner countLeaf

    countInner <- block `named` "count_inner"
    numElements' <- zext numElements i64
    count <- loopChildren n i64 numElements' $ \entryCount child -> mdo
      childNodeCount <- call countEntries [(child, [])]
      add entryCount childNodeCount

    countLeaf <- block `named` "count_leaf"
    ret numElements

  pure ()

mkNodeIsEmpty :: ModuleCodegen ()
mkNodeIsEmpty = mdo
  node <- typeOf Node

  function "node_is_empty" [(ptr node, "node")] i1 $ \[n] -> mdo
    numElements <- deref (metaOf ->> numElemsOf) n
    ret =<< numElements `eq` int16 0

  pure ()

mkNodeIsFull :: ModuleCodegen ()
mkNodeIsFull = mdo
  metadata <- asks meta
  let numberOfKeys = int16 $ toInteger $ numKeys metadata
  node <- typeOf Node

  function "node_is_full" [(ptr node, "node")] i1 $ \[n] -> mdo
    numElements <- deref (metaOf ->> numElemsOf) n
    ret =<< numElements `eq` numberOfKeys

  pure ()

mkNodeSplitPoint :: ModuleCodegen ()
mkNodeSplitPoint = mdo
  nodeSize <- typeOf NodeSize
  metadata <- asks meta
  let numberOfKeys = int16 $ toInteger $ numKeys metadata

  function "node_split_point" [] nodeSize $ \_ -> mdo
    a' <- mul (int16 3) numberOfKeys
    a <- fdiv a' (int16 4)
    b <- sub numberOfKeys (int16 2)
    ret =<< minimum a b

  pure ()

-- NOTE: only works for unsigned integers!
minimum :: Operand -> Operand -> IRCodegen Operand
minimum a b = do
  isLessThan <- a `ult` b
  select isLessThan a b

loopChildren :: Operand
             -> Type
             -> Operand
             -> (Operand -> Operand -> IRCodegen Operand)
             -> IRCodegen Operand
loopChildren n ty beginValue f = mdo
  innerNode <- typeOf InnerNode
  inner <- n `bitcast` ptr innerNode

  result <- allocate ty beginValue
  numElements <- deref (metaOf ->> numElemsOf) n
  forLoop (int16 0) (`ule` numElements) (add (int16 1)) $ \i -> mdo
    currentResult <- load result 0
    child <- deref (childAt i) inner
    updatedResult <- f currentResult child
    store result 0 updatedResult

  load result 0

leafNodeTypeVal, innerNodeTypeVal :: Operand
leafNodeTypeVal = bit 0
innerNodeTypeVal = bit 1

data Types
  = Types
  { btreeTy :: Type
  , iteratorTy :: Type
  , nodeSizeTy :: Type
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

type IRCodegen = LLVM.IRCodegen CGState

type ModuleCodegen = LLVM.ModuleCodegen CGState

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

ptrSize :: Meta -> Word64
ptrSize md = case arch md of
  X86 -> 4
  X64 -> 8


data Index
  = NodeIdx
  | InnerNodeIdx
  | MetaIdx
  | ValueIdx
  | PositionIdx
  | NumElemsIdx
  | NodeTypeIdx
  | ArrayOf Index

metaOf :: Path 'NodeIdx 'MetaIdx
metaOf = mkPath [int32 0]

valuesOf :: Path 'NodeIdx ('ArrayOf ValueIdx)
valuesOf = mkPath [int32 1]

valueAt :: Operand -> Path 'NodeIdx 'ValueIdx
valueAt idx = mkPath [int32 1, idx]

parentOf :: Path 'MetaIdx 'NodeIdx
parentOf = mkPath [int32 0]

posInParentOf :: Path 'MetaIdx 'PositionIdx
posInParentOf = mkPath [int32 1]

numElemsOf :: Path 'MetaIdx 'NumElemsIdx
numElemsOf = mkPath [int32 2]

nodeTypeOf :: Path 'MetaIdx 'NodeTypeIdx
nodeTypeOf = mkPath [int32 3]

baseOf :: Path 'InnerNodeIdx 'NodeIdx
baseOf = mkPath [int32 0]

childrenOf :: Path 'InnerNodeIdx ('ArrayOf NodeIdx)
childrenOf = mkPath [int32 1]

childAt :: Operand -> Path 'InnerNodeIdx 'NodeIdx
childAt idx = mkPath [int32 1, idx]

data DataType
  = NodeType
  | Node
  | InnerNode
  | Value
  | NodeSize

-- TODO: remove this hack and replace with proper usage of LLVM Datalayout class
sizeOf :: MonadReader CGState m => DataType -> m Word64
sizeOf dt = do
  md <- asks meta
  pure $ fromIntegral $ f (numColumns md) (arch md)
  where
    f columnCount = \case
      X86 -> case dt of
        NodeType -> 1
        Node -> 252
        InnerNode -> 336
        Value -> columnCount * 4
        NodeSize -> 2
      X64 -> case dt of
        NodeType -> 1
        Node -> 256
        InnerNode -> 424
        Value -> columnCount * 4
        NodeSize -> 2

typeOf :: MonadReader CGState m => DataType -> m Type
typeOf dt =
  let getType = case dt of
        Node -> nodeTy
        NodeType -> nodeTypeTy
        InnerNode -> innerNodeTy
        Value -> valueTy
        NodeSize -> nodeSizeTy
   in getType <$> asks types

memset :: Operand -> Word8 -> Word64 -> IRCodegen ()
memset p val byteCount = do
  memsetFn <- asks (extMemset . externals)
  p' <- p `bitcast` ptr i8
  call memsetFn [ (p', [])
                , (int8 0, [])
                , (int64 (fromIntegral byteCount), [])
                , (bit 0, [])
                ]
  pure ()

