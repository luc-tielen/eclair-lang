{-# LANGUAGE RecursiveDo, FlexibleContexts, ScopedTypeVariables #-}

module Eclair.Runtime.BTree
  ( Meta(..)
  , SearchIndex
  , SearchType(..)
  , codegen
  ) where

import Protolude hiding (Type, Meta, compare, swap, void, bit, typeOf, and)
import Control.Arrow ((&&&))
import Control.Monad.Morph
import Control.Monad.Fix
import qualified Data.Map as Map
import Data.Functor.Foldable hiding (hoist)
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Type
import LLVM.AST.Operand (Operand(..))
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Combinators
import Eclair.Runtime.LLVM
import Eclair.Runtime.Hash
import Eclair.Runtime.Store (Functions(..))
import Prettyprinter


data Meta
  = Meta
  { numColumns :: Int        -- Amount of columns each node has
  , index :: SearchIndex     -- Which columns are used to index values
  , blockSize :: Word64      -- Number of bytes per btree node
  , searchType :: SearchType -- Search strategy used in a single node
  }
  deriving stock (Eq, Show)
  deriving stock Generic
  deriving ToHash via HashWithPrefix "btree" Meta

instance Pretty Meta where
  pretty meta =
    "num_columns=" <> pretty (numColumns meta) <> comma <+>
    -- TODO: use "withCommas"
    "index=" <> brackets (foldMap ((<> comma) . pretty) (index meta)) <> comma <+>
    "block_size=" <> pretty (blockSize meta) <> comma <+>
    "search_type=" <> pretty (searchType meta)

type Column = Int

type SearchIndex = [Column]

data SearchType = Linear | Binary
  deriving stock (Eq, Show)
  deriving stock (Generic, Enum)
  deriving ToHash via HashEnum SearchType

instance Pretty SearchType where
  pretty Linear = "linear"
  pretty Binary = "binary"

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

data Sizes
  = Sizes
  { pointerSize :: Word64
  , valueSize :: Word64
  , nodeDataSize :: Word64
  , leafNodeSize :: Word64
  , innerNodeSize :: Word64
  }

data CGState
  = CGState
  { meta :: Meta
  , types :: Types
  , typeSizes :: Sizes
  , externals :: Externals
  }
  deriving ToHash via HashOnly "meta" CGState

type IRCodegen = IRBuilderT ModuleCodegen

type ModuleCodegen = ReaderT CGState ModuleBuilder


codegen :: Meta -> ModuleBuilderT IO Functions
codegen settings = do
  sizes <- computeSizes settings
  hoist intoIO $ do
    tys <- runReaderT (generateTypes sizes) settings
    exts <- mkExternals
    runReaderT generateFunctions $ CGState settings tys sizes exts
  where intoIO = pure . runIdentity

mkExternals :: ModuleBuilder Externals
mkExternals = do
  malloc <- extern "malloc" [i32] (ptr i8)
  free <- extern "free" [ptr i8] void
  memsetFn <- extern "llvm.memset.p0i8.i64" [ptr i8, i8, i64, i1] void
  pure $ Externals malloc free memsetFn

computeSizes :: Meta -> ModuleBuilderT IO Sizes
computeSizes settings = do
  let nodeDataTy = wrap
        [ -- Next type doesn't matter here, but we need to break the
          -- cyclic loop or Haskell will throw an exception.
          ptrTy   -- parent
        , i16     -- position_in_parent
        , i16     -- num_elements
        , i1      -- node type
        ]
      ptrTy = wrap [ptr i8]
      valueType = wrap [ArrayType (fromIntegral $ numColumns settings) i32]
  ptrSize <- sizeOfType ("pointer_t", ptrTy)
  valueSz <- sizeOfType ("value_t", valueType)
  nodeDataSz <- sizeOfType ("node_data_t", nodeDataTy)
  let numKeys' = numKeysHelper settings nodeDataSz valueSz
      nodeType = wrap [nodeDataTy, ArrayType numKeys' valueType]
      innerNodeType = wrap [nodeType, ArrayType (numKeys' + 1) (ptr nodeType)]
  leafNodeSz <- sizeOfType ("leaf_node_t", nodeType)
  innerNodeSz <- sizeOfType ("inner_node_t", innerNodeType)
  pure $ Sizes ptrSize valueSz nodeDataSz leafNodeSz innerNodeSz
  where
    wrap = StructureType False

generateTypes :: (MonadModuleBuilder m, MonadReader Meta m, MonadFix m)
              => Sizes -> m Types
generateTypes sizes = mdo
  meta <- ask
  let numKeys' = numKeys meta sizes

  columnTy <- mkType "column_t" i32
  valueTy <- mkType "value_t" $ ArrayType (fromIntegral $ numColumns meta) columnTy
  positionTy <- mkType "position_t" i16
  nodeSizeTy <- mkType "node_size_t" i16  -- Note: used to be size_t/i64
  nodeTypeTy <- mkType "node_type_t" i1
  let nodeDataName = "node_data_t"
  nodeDataTy <- mkType nodeDataName $
    mkStruct [ ptr nodeTy  -- parent
             , positionTy  -- position_in_parent
             , nodeSizeTy  -- num_elements
             , nodeTypeTy  -- node type
             ]
  nodeTy <- mkType "node_t" $
    mkStruct [ nodeDataTy                  -- meta
             , ArrayType numKeys' valueTy  -- values
             ]
  leafNodeTy <- mkType "leaf_node_t" nodeTy
  innerNodeTy <- mkType "inner_node_t" $
    mkStruct [ nodeTy                                 -- base
             , ArrayType (numKeys' + 1) (ptr nodeTy)  -- children
             ]
  btreeIteratorTy <- mkType "btree_iterator_t" $
    mkStruct [ ptr nodeTy  -- current
             , positionTy  -- value pos
             ]
  btreeTy <- mkType "btree_t" $
    mkStruct [ ptr nodeTy  -- root
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
  where mkStruct = StructureType False

generateFunctions :: ModuleCodegen Functions
generateFunctions = mdo
  compareValues <- mkCompare
  nodeNew <- mkNodeNew
  nodeDelete <- mkNodeDelete
  splitPoint <- mkNodeSplitPoint
  split <- mkSplit nodeNew splitPoint growParent
  growParent <- mkGrowParent nodeNew insertInner
  insertInner <- mkInsertInner rebalanceOrSplit
  rebalanceOrSplit <- mkRebalanceOrSplit split
  iterInit <- mkIteratorInit
  iterInitEnd <- mkIteratorInitEnd iterInit
  iterIsEqual <- mkIteratorIsEqual
  iterCurrent <- mkIteratorCurrent
  iterNext <- mkIteratorNext
  searchLowerBound <- mkLinearSearchLowerBound compareValues
  searchUpperBound <- mkLinearSearchUpperBound compareValues
  btreeInitEmpty <- mkBtreeInitEmpty
  btreeInit <- mkBtreeInit btreeInsertRange
  btreeDestroy <- mkBtreeDestroy btreeClear
  isEmptyTree <- mkBtreeIsEmpty
  btreeInsert <- mkBtreeInsertValue nodeNew rebalanceOrSplit compareValues searchLowerBound searchUpperBound isEmptyTree
  btreeInsertRange <- mkBtreeInsertRange iterIsEqual iterCurrent iterNext btreeInsert
  btreeBegin <- mkBtreeBegin
  btreeEnd <- mkBtreeEnd iterInitEnd
  btreeContains <- mkBtreeContains iterIsEqual btreeFind btreeEnd
  btreeFind <- mkBtreeFind isEmptyTree searchLowerBound compareValues iterInit iterInitEnd
  btreeLowerBound <- mkBtreeLowerBound isEmptyTree iterInit iterInitEnd searchLowerBound compareValues
  btreeUpperBound <- mkBtreeUpperBound isEmptyTree iterInit iterInitEnd searchUpperBound
  btreeClear <- mkBtreeClear nodeDelete
  btreeSwap <- mkBtreeSwap

  tree <- typeOf BTree
  iter <- typeOf Iterator
  value <- typeOf Value
  pure Functions
        { fnInit = btreeInit
        , fnInitEmpty = btreeInitEmpty
        , fnDestroy = btreeDestroy
        , fnPurge = btreeClear
        , fnSwap = btreeSwap
        , fnBegin = btreeBegin
        , fnEnd = btreeEnd
        , fnInsert = btreeInsert
        , fnInsertRange = btreeInsertRange
        , fnIsEmpty = isEmptyTree
        , fnLowerBound = btreeLowerBound
        , fnUpperBound = btreeUpperBound
        , fnContains = btreeContains
        , fnIterIsEqual = iterIsEqual
        , fnIterCurrent = iterCurrent
        , fnIterNext = iterNext
        , typeObj = tree
        , typeIter = iter
        , typeValue = value
        }

mkCompare :: ModuleCodegen Operand
mkCompare = do
  (tys, settings) <- asks (types &&& meta)
  let column = columnTy tys
      value = valueTy tys
  compare <- def "compare" [(column, "lhs"), (column, "rhs")] i8 $ \[lhs, rhs] -> mdo
    result1 <- lhs `ult` rhs
    if' result1 $
      ret $ int8 (-1)
    result2 <- lhs `ugt` rhs
    ret =<< select result2 (int8 1) (int8 0)

  def "compare_values" [(ptr value, "lhs"), (ptr value, "rhs")] i8 $ \[lhs, rhs] -> mdo
    let columns = map fromIntegral $ index settings
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

  sizes <- asks typeSizes
  let numKeys' = numKeys md sizes
      ptrSize = pointerSize sizes
      valuesByteCount = numKeys' * valueSize sizes
      leafSize = int32 . toInteger $ leafNodeSize sizes
      innerSize = int32 . toInteger $ innerNodeSize sizes

  malloc <- asks (extMalloc . externals)

  def "node_new" [(nodeType, "type")] (ptr node) $ \[ty] -> mdo
    structSize <- select ty leafSize innerSize
    memory <- call malloc [(structSize, [])]
    n <- memory `bitcast` ptr node

    assign (metaOf ->> parentOf) n (nullPtr node)
    assign (metaOf ->> posInParentOf) n (int16 0)
    assign (metaOf ->> numElemsOf) n (int16 0)
    assign (metaOf ->> nodeTypeOf) n ty

    valuesPtr <- addr valuesOf n
    memset valuesPtr 0 valuesByteCount

    isInner <- ty `eq` innerNodeTypeVal
    if' isInner $ mdo
      inner <- n `bitcast` ptr innerNode
      let childrenByteCount = (numKeys' + 1) * ptrSize
      childrenPtr <- addr childrenOf inner
      memset childrenPtr 0 childrenByteCount

    ret n

mkNodeDelete :: ModuleCodegen Operand
mkNodeDelete = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  free <- asks (extFree . externals)

  nodeDelete <- def "node_delete" [(ptr node, "node")] void $ \[n] -> mdo
    nodeTy <- deref (metaOf ->> nodeTypeOf) n
    isInner <- nodeTy `eq` innerNodeTypeVal
    if' isInner $ do  -- Delete children of inner node
      inner <- n `bitcast` ptr innerNode

      numElements <- deref (metaOf ->> numElemsOf) n
      loopFor (int16 0) (`ule` numElements) (add (int16 1)) $ \i -> mdo
        child <- deref (childAt i) inner
        isNotNull <- child `ne` nullPtr node
        if' isNotNull $
          call nodeDelete [(child, [])]

    memory <- n `bitcast` ptr i8
    _ <- call free [(memory, [])]
    pure ()

  pure nodeDelete

mkNodeSplitPoint :: ModuleCodegen Operand
mkNodeSplitPoint = mdo
  nodeSize <- typeOf NodeSize
  numberOfKeys <- numKeysAsOperand

  def "node_split_point" [] nodeSize $ \_ -> mdo
    a' <- mul (int16 3) numberOfKeys
    a <- udiv a' (int16 4)
    b <- sub numberOfKeys (int16 2)
    ret =<< minimum' Unsigned a b

mkSplit :: Operand -> Operand -> Operand -> ModuleCodegen Operand
mkSplit nodeNew nodeSplitPoint growParent = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  numberOfKeys <- numKeysAsOperand

  def "node_split" [(ptr node, "node"), (ptr (ptr node), "root")] void $ \[n, root] -> mdo
    -- TODO: how to do assertions in LLVM?
    -- assert(n->meta.num_elements == NUM_KEYS);
    splitPoint <- call nodeSplitPoint []
    splitPoint' <- add (int16 1) splitPoint
    ty <- deref (metaOf ->> nodeTypeOf) n
    -- Create a new sibling node and move some of the data to sibling
    sibling <- call nodeNew [(ty, [])]
    jPtr <- allocate i16 (int16 0)
    loopFor splitPoint' (`ult` numberOfKeys) (add (int16 1)) $ \i -> mdo
      j <- load jPtr 0
      assign (valueAt j) sibling =<< deref (valueAt i) n
      store jPtr 0 =<< add (int16 1) j

    isInner <- ty `eq` innerNodeTypeVal
    if' isInner $ mdo
      iSibling <- sibling `bitcast` ptr innerNode
      iN <- n `bitcast` ptr innerNode

      store jPtr 0 (int16 0)
      loopFor splitPoint' (`ult` numberOfKeys) (add (int16 1)) $ \i -> mdo
        j <- load jPtr 0
        iChild <- deref (childAt i) iN
        assign (metaOf ->> parentOf) iChild sibling
        assign (metaOf ->> numElemsOf) iChild j
        assign (childAt j) iSibling iChild
        store jPtr 0 =<< add (int16 1) j

    assign (metaOf ->> numElemsOf) n splitPoint
    siblingNumKeys <- sub numberOfKeys splitPoint >>= flip sub (int16 1)
    assign (metaOf ->> numElemsOf) sibling siblingNumKeys

    _ <- call growParent [(n, []), (root, []), (sibling, [])]
    pure ()

mkGrowParent :: Operand -> Operand -> ModuleCodegen Operand
mkGrowParent nodeNew insertInner = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode

  def "node_grow_parent" [(ptr node, "node"), (ptr (ptr node), "root"), (ptr node, "sibling")] void $
    \[n, root, sibling] -> mdo
    parent <- deref (metaOf ->> parentOf) n
    isNull <- parent `eq` nullPtr node
    numElems <- deref (metaOf ->> numElemsOf) n
    condBr isNull createNewRoot insertNewNodeInParent

    createNewRoot <- block `named` "create_new_root"
    -- TODO: assert(n == *root)
    newRoot <- call nodeNew [(innerNodeTypeVal, [])]
    iNewRoot <- newRoot `bitcast` ptr innerNode
    assign (metaOf ->> numElemsOf) newRoot (int16 1)
    lastValueOfN <- deref (valueAt numElems) n
    assign (valueAt (int16 0)) newRoot lastValueOfN
    assign (childAt (int16 0)) iNewRoot n
    assign (childAt (int16 1)) iNewRoot sibling

    assign (metaOf ->> parentOf) n newRoot
    assign (metaOf ->> parentOf) sibling newRoot
    assign (metaOf ->> posInParentOf) n (int16 0)  -- TODO: why missing in souffle code? default initialized?
                                                   -- also: why is num elements of n not decremented?
    assign (metaOf ->> posInParentOf) sibling (int16 1)
    store root 0 newRoot
    retVoid

    insertNewNodeInParent <- block `named` "insert_new_node_in_parent"
    pos <- deref (metaOf ->> posInParentOf) n
    lastValuePtr <- addr (valueAt numElems) n
    _ <- call insertInner $ (, []) <$> [parent, root, pos, n, lastValuePtr, sibling]
    retVoid

mkInsertInner :: Operand -> ModuleCodegen Operand
mkInsertInner rebalanceOrSplit = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  nodeSize <- typeOf NodeSize
  value <- typeOf Value
  let args = [ (ptr node, "node"), (ptr (ptr node), "root")
             , (nodeSize, "pos"), (ptr node, "predecessor")
             , (ptr value, "key"), (ptr node, "new_node")
             ]
  numberOfKeys <- numKeysAsOperand

  insertInner <- def "node_insert_inner" args void $
    \[n, root, pos, predecessor, key, newNode] -> mdo
    -- Need to allocate pos on the stack, otherwise pos updates are
    -- not visible later on!
    posPtr <- allocate nodeSize pos

    numElems <- deref (metaOf ->> numElemsOf) n
    needsRebalanceOrSplit <- numElems `uge` numberOfKeys

    if' needsRebalanceOrSplit $ do
      position' <- load posPtr 0
      position'' <- sub position' =<< call rebalanceOrSplit ((,[]) <$> [n, root, pos])
      store posPtr 0 position''
      numElems' <- deref (metaOf ->> numElemsOf) n  -- NOTE: n might be updated in rebalanceOrSplit
      needsInsertInNewNode <- position'' `ugt` numElems'

      if' needsInsertInNewNode $ do
        -- Insertion needs to be done in new sibling node:
        pos''' <- sub position'' numElems' >>= flip sub (int16 1)
        store posPtr 0 pos'''
        parent <- deref (metaOf ->> parentOf) n >>= (`bitcast` ptr innerNode)
        siblingPos <- add (int16 1) =<< deref (metaOf ->> posInParentOf) n
        sibling <- deref (childAt siblingPos) parent
        _ <- call insertInner $ (, []) <$> [sibling, root, pos''', predecessor, key, newNode]
        retVoid

    -- Move bigger keys one forward
    iN <- n `bitcast` ptr innerNode
    numElems'' <- deref (metaOf ->> numElemsOf) n
    startIdx <- sub numElems'' (int16 1)
    pos' <- load posPtr 0
    loopFor startIdx (`uge` pos') (`sub` int16 1) $ \i -> mdo
      j <- add i (int16 1)
      k <- add i (int16 2)
      assign (valueAt j) n =<< deref (valueAt i) n
      assign (childAt k) iN =<< deref (childAt j) iN
      childK <- deref (childAt k) iN
      increment int16 (metaOf ->> posInParentOf) childK

    -- TODO: assert(i_n->children[pos] == predecessor);

    -- Insert new element
    assign (valueAt pos') n =<< load key 0
    pos'' <- add pos' (int16 1)
    assign (childAt pos'') iN newNode
    assign (metaOf ->> parentOf) newNode n
    assign (metaOf ->> posInParentOf) newNode pos''
    increment int16 (metaOf ->> numElemsOf) n

  pure insertInner

mkRebalanceOrSplit :: Operand -> ModuleCodegen Operand
mkRebalanceOrSplit splitFn = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  nodeSize <- typeOf NodeSize

  numberOfKeys <- numKeysAsOperand

  let args = [(ptr node, "node"), (ptr (ptr node), "root"), (nodeSize, "idx")]
  def "node_rebalance_or_split" args nodeSize $ \[n, root, idx] -> mdo
    -- TODO assert(n->meta.num_elements == NUM_KEYS);

    parent <- deref (metaOf ->> parentOf) n >>= (`bitcast` ptr innerNode)
    pos <- deref (metaOf ->> posInParentOf) n
    hasParent <- parent `ne` nullPtr node
    posGTZero <- pos `ugt` int16 0
    shouldRebalance <- and hasParent posGTZero
    condBr shouldRebalance rebalance split

    rebalance <- block `named` "rebalance"
    -- Option A) re-balance data
    pos' <- sub pos (int16 1)
    left <- deref (childAt pos') parent

    -- Compute amount of elements movable to the left
    leftSlotsOpen <- calculateLeftSlotsOpen numberOfKeys left idx
    hasOpenLeftSlots <- leftSlotsOpen `ugt` int16 0
    if' hasOpenLeftSlots $ do
      splitPos <- deref (metaOf ->> posInParentOf) n >>= (`sub` int16 1)
      splitter <- addr (baseOf ->> valueAt splitPos) parent
      splitterValue <- load splitter 0

      -- Move keys to left node
      leftNumElems <- deref (metaOf ->> numElemsOf) left
      assign (valueAt leftNumElems) left splitterValue

      leftSlotsOpen' <- sub leftSlotsOpen (int16 1)
      loopFor (int16 0) (`ult` leftSlotsOpen') (add (int16 1)) $ \i -> do
        j <- add leftNumElems (int16 1) >>= add i
        assign (valueAt j) left =<< deref (valueAt i) n

      store splitter 0 =<< deref (valueAt leftSlotsOpen') n

      -- Shift keys in this node to the left
      numElemsN <- deref (metaOf ->> numElemsOf) n
      idxEnd <- sub numElemsN leftSlotsOpen
      loopFor (int16 0) (`ult` idxEnd) (add (int16 1)) $ \i -> do
        -- TODO memmove possible?
        j <- add i leftSlotsOpen
        assign (valueAt i) n =<< deref (valueAt j) n

      -- And children (if necessary)
      isInnerNode <- deref (metaOf ->> nodeTypeOf) n >>= (`eq` innerNodeTypeVal)
      if' isInnerNode $ do
        iN <- n `bitcast` ptr innerNode
        iLeft <- left `bitcast` ptr innerNode

        -- Move children
        loopFor (int16 0) (`ult` leftSlotsOpen) (add (int16 1)) $ \i -> do
          leftNumElems' <- deref (metaOf ->> numElemsOf) left
          leftPos <- add leftNumElems' (int16 1) >>= add i
          -- TODO: check next part against C++ code
          assign (childAt leftPos) iLeft =<< deref (childAt i) iN
          leftChild <- deref (childAt leftPos) iLeft
          assign (metaOf ->> parentOf) leftChild left
          assign (metaOf ->> posInParentOf) leftChild leftPos

        -- Shift child pointer to the left + update position
        endIdx <- sub numElemsN leftSlotsOpen >>= add (int16 1)
        loopFor (int16 0) (`ult` endIdx) (add (int16 1)) $ \i -> do
          j <- add i leftSlotsOpen
          assign (childAt i) iN =<< deref (childAt j) iN
          child <- deref (childAt i) iN
          assign (metaOf ->> posInParentOf) child i

      -- Update node sizes
      update (metaOf ->> numElemsOf) left (`add` leftSlotsOpen)
      update (metaOf ->> numElemsOf) n (`sub` leftSlotsOpen)
      ret leftSlotsOpen

    br split
    split <- block `named` "split"
    -- Option B) split
    _ <- call splitFn $ (,[]) <$> [n, root]
    ret (int16 0)  -- No re-balancing
  where
    calculateLeftSlotsOpen numberOfKeys left idx = do
      numElems <- deref (metaOf ->> numElemsOf) left
      openSlots <- sub numberOfKeys numElems
      isLessThan <- openSlots `slt` idx
      select isLessThan openSlots idx

mkIteratorInit :: ModuleCodegen Operand
mkIteratorInit = do
  iter <- typeOf Iterator
  node <- typeOf Node
  nodeSize <- typeOf NodeSize
  let args = [(ptr iter, "iter"), (ptr node, "cur"), (nodeSize, "pos")]

  def "iterator_init" args void $ \[it, cur, pos] -> do
    assign currentPtrOf it cur
    assign valuePosOf it pos

mkIteratorInitEnd :: Operand -> ModuleCodegen Operand
mkIteratorInitEnd iterInit = do
  iter <- typeOf Iterator
  node <- typeOf Node

  def "iterator_end_init" [(ptr iter, "iter")] void $ \[it] -> do
    _ <- call iterInit $ (,[]) <$> [it, nullPtr node, int16 0]
    retVoid

mkIteratorIsEqual :: ModuleCodegen Operand
mkIteratorIsEqual = do
  iter <- typeOf Iterator

  def "iterator_is_equal" [(ptr iter, "lhs"), (ptr iter, "rhs")] i1 $ \[lhs, rhs] -> mdo
    currentLhs <- deref currentPtrOf lhs
    currentRhs <- deref currentPtrOf rhs

    isDifferentPtrs <- currentLhs `ne` currentRhs
    if' isDifferentPtrs $
      ret (bit 0)

    valuePosLhs <- deref valuePosOf lhs
    valuePosRhs <- deref valuePosOf rhs
    ret =<< valuePosLhs `eq` valuePosRhs

mkIteratorCurrent :: ModuleCodegen Operand
mkIteratorCurrent = do
  iter <- typeOf Iterator
  value <- typeOf Value

  def "iterator_current" [(ptr iter, "iter")] (ptr value) $ \[it] -> mdo
    valuePos <- deref valuePosOf it
    currentNode <- deref currentPtrOf it
    ret =<< addr (valueAt valuePos) currentNode

mkIteratorNext :: ModuleCodegen Operand
mkIteratorNext = do
  iter <- typeOf Iterator

  def "iterator_next" [(ptr iter, "iter")] void $ \[it] -> mdo
    current <- deref currentPtrOf it
    isLeaf <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` leafNodeTypeVal)
    if' isLeaf $ do
      leafIterNext it
      retVoid

    innerIterNext it
  where
    leafIterNext iter = mdo
      node <- typeOf Node
      -- Case 1: Still elements left to iterate -> increment position
      increment int16 valuePosOf iter
      valuePos <- deref valuePosOf iter
      current <- deref currentPtrOf iter
      numElems <- deref (metaOf ->> numElemsOf) current
      hasNextInLeaf <- valuePos `ult` numElems
      if' hasNextInLeaf
        retVoid

      -- Case 2: at right-most element -> go to next inner node
      let loopCondition = do
            isNotNull <- deref currentPtrOf iter >>= (`ne` nullPtr node)
            pos' <- deref valuePosOf iter
            current' <- deref currentPtrOf iter
            numElems' <- deref (metaOf ->> numElemsOf) current'
            atEnd <- pos' `eq` numElems'
            isNotNull `and` atEnd
      loopWhile loopCondition $ do
        current' <- deref currentPtrOf iter
        assign valuePosOf iter =<< deref (metaOf ->> posInParentOf) current'
        assign currentPtrOf iter =<< deref (metaOf ->> parentOf) current'
    innerIterNext iter = mdo
      node <- typeOf Node
      innerNode <- typeOf InnerNode
      -- Case 3: Go to left most child in inner node
      nextPos <- deref valuePosOf iter >>= add (int16 1)
      iCurrent <- deref currentPtrOf iter >>= (`bitcast` ptr innerNode)
      currentPtr <- allocate (ptr node) =<< deref (childAt nextPos) iCurrent
      let loopCondition' = do
            ty <- deref (metaOf ->> nodeTypeOf) =<< load currentPtr 0
            ty `eq` innerNodeTypeVal
      loopWhile loopCondition' $ do
        iCurrent' <- load currentPtr 0 >>= (`bitcast` ptr innerNode)
        firstChild <- deref (childAt (int16 0)) iCurrent'
        store currentPtr 0 firstChild

      assign currentPtrOf iter =<< load currentPtr 0
      assign valuePosOf iter (int16 0)

mkLinearSearchLowerBound :: Operand -> ModuleCodegen Operand
mkLinearSearchLowerBound compareValues = do
  value <- typeOf Value
  let args = [(ptr value, "val"), (ptr value, "current"), (ptr value, "end")]

  def "linear_search_lower_bound" args (ptr value) $ \[val, curr, end] -> mdo
    -- Finds an iterator to first element not less than given value.
    currentPtr <- allocate (ptr value) curr
    let loopCondition = do
          current <- load currentPtr 0
          current `ne` end
    loopWhile loopCondition $ mdo
      current <- load currentPtr 0
      result <- call compareValues [(current, []), (val, [])]
      isGtOrEqThan <- result `ne` int8 (-1)
      if' isGtOrEqThan $
        ret current

      current' <- gep current [int32 1]
      store currentPtr 0 current'

    ret end

mkLinearSearchUpperBound :: Operand -> ModuleCodegen Operand
mkLinearSearchUpperBound compareValues = do
  value <- typeOf Value
  let args = [(ptr value, "val"), (ptr value, "current"), (ptr value, "end")]

  def "linear_search_upper_bound" args (ptr value) $ \[val, curr, end] -> mdo
    -- Finds an iterator to first element that is greater than given value.
    currentPtr <- allocate (ptr value) curr
    let loopCondition = do
          current <- load currentPtr 0
          current `ne` end
    loopWhile loopCondition $ mdo
      current <- load currentPtr 0
      result <- call compareValues [(current, []), (val, [])]
      isGreaterThan <- result `eq` int8 1
      if' isGreaterThan $
        ret current

      current' <- gep current [int32 1]
      store currentPtr 0 current'

    ret end

mkBtreeInitEmpty :: ModuleCodegen Operand
mkBtreeInitEmpty = do
  tree <- typeOf BTree
  node <- typeOf Node

  def "btree_init_empty" [(ptr tree, "tree")] void $ \[t] -> mdo
    assign rootPtrOf t (nullPtr node)
    assign firstPtrOf t (nullPtr node)

mkBtreeInit :: Operand -> ModuleCodegen Operand
mkBtreeInit btreeInsertRange = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  let args = [(ptr tree, "tree"), (ptr iter, "start"), (ptr iter, "end")]

  def "btree_init" args void $ \[t, start, end] -> mdo
    _ <- call btreeInsertRange $ (,[]) <$> [t, start, end]
    pure ()

mkBtreeDestroy :: Operand -> ModuleCodegen Operand
mkBtreeDestroy btreeClear = do
  tree <- typeOf BTree

  def "btree_destroy" [(ptr tree, "tree")] void $ \[t] -> do
    _ <- call btreeClear [(t, [])]
    pure ()

mkBtreeIsEmpty :: ModuleCodegen Operand
mkBtreeIsEmpty = do
  tree <- typeOf BTree
  node <- typeOf Node

  def "btree_is_empty" [(ptr tree, "tree")] i1 $ \[t] -> do
    root <- deref rootPtrOf t
    ret =<< root `eq` nullPtr node

mkBtreeInsertValue :: Operand -> Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeInsertValue nodeNew rebalanceOrSplit compareValues searchLowerBound searchUpperBound isEmptyTree = do
  tree <- typeOf BTree
  node <- typeOf Node
  value <- typeOf Value
  numberOfKeys <- numKeysAsOperand

  def "btree_insert_value" [(ptr tree, "tree"), (ptr value, "val")] i1 $ \[t, val] -> mdo
    isEmpty <- call isEmptyTree [(t, [])]
    condBr isEmpty emptyCase nonEmptyCase

    emptyCase <- block `named` "empty"
    leaf <- call nodeNew [(leafNodeTypeVal, [])]
    assign (metaOf ->> numElemsOf) leaf (int16 1)
    assign (valueAt (int16 0)) leaf =<< load val 0

    assign rootPtrOf t leaf
    assign firstPtrOf t leaf
    br inserted

    nonEmptyCase <- block `named` "non_empty"
    -- Insert using iterative approach
    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t
    loop $ mdo
      loopBlock <- currentBlock
      current <- load currentPtr 0
      isInner <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` innerNodeTypeVal)
      condBr isInner inner leaf

      inner <- block `named` "inner"
      insertInNonEmptyInnerNode loopBlock noInsert currentPtr current val

      leaf <- block `named` "leaf"
      insertInNonEmptyLeafNode noInsert inserted t currentPtr current val numberOfKeys

    noInsert <- block `named` "no_insert"
    ret (bit 0)

    inserted <- block `named` "inserted_new_value"
    ret (bit 1)
  where
    insertInNonEmptyInnerNode loopBlock noInsert currentPtr current val = mdo
      innerNode <- typeOf InnerNode

      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchLowerBound $ (,[]) <$> [val, first, last]
      idx <- pointerDiff i16 pos first
      notLast <- pos `ne` last
      valueAtPos <- gep pos [int32 0]
      isEqual <- (int8 0 `eq`) =<< call compareValues ((,[]) <$> [valueAtPos, val])  -- Can we do a weak compare just by using pointers here?
      alreadyInserted <- notLast `and` isEqual
      condBr alreadyInserted noInsert continueInsert

      continueInsert <- block `named` "inner_continue_insert"
      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent
      br loopBlock

    insertInNonEmptyLeafNode noInsert inserted t currentPtr current val numberOfKeys = mdo
      -- Rest is for leaf nodes
      innerNode <- typeOf InnerNode

      -- TODO: assert(current->meta.type == LEAF_NODE);
      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchUpperBound $ (,[]) <$> [val, first, last]
      idxPtr <- allocate i16 =<< pointerDiff i16 pos first
      notFirst <- pos `ne` first
      valueAtPrevPos <- gep pos [int32 (-1)]
      isEqual <- (int8 0 `eq`) =<< call compareValues ((,[]) <$> [valueAtPrevPos, val])  -- Can we do a weak compare just by using pointers here?
      alreadyInserted <- notFirst `and` isEqual
      condBr alreadyInserted noInsert continueInsert

      continueInsert <- block `named` "leaf_continue_insert"
      nodeIsFull <- numElems `uge` numberOfKeys
      condBr nodeIsFull split noSplit

      split <- block `named` "split"
      root <- addr rootPtrOf t
      idx <- load idxPtr 0
      res <- call rebalanceOrSplit $ (,[]) <$> [current, root, idx]
      idx' <- sub idx res
      store idxPtr 0 idx'

      -- Insert in right fragment if needed
      shouldInsertRight <- idx' `ugt` numElems
      if' shouldInsertRight $ do
        numElems' <- add numElems (int16 1)
        idx'' <- sub idx' numElems'
        store idxPtr 0 idx''
        parent <- deref (metaOf ->> parentOf) current >>= (`bitcast` ptr innerNode)
        nextPos <- deref (metaOf ->> posInParentOf) current >>= add (int16 1)
        store currentPtr 0 =<< deref (childAt nextPos) parent

      br noSplit

      noSplit <- block `named` "no_split"
      -- No split -> move keys and insert new element
      idx''' <- load idxPtr 0
      numElems'' <- deref (metaOf ->> numElemsOf) current  -- Might've been updated in the meantime
      loopFor numElems'' (`ugt` idx''') (`sub` int16 1) $ \j -> do
        -- TODO: memmove possible?
        j' <- sub j (int16 1)
        assign (valueAt j) current =<< deref (valueAt j') current

      assign (valueAt idx''') current =<< load val 0
      update (metaOf ->> numElemsOf) current (add (int16 1))
      br inserted

mkBtreeInsertRange :: Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeInsertRange iterIsEqual iterCurrent iterNext btreeInsertValue = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  let args = [(ptr tree, "tree"), (ptr iter, "begin"), (ptr iter, "end")]

  def "btree_insert_range" args void $ \[t, begin, end] -> do
    let loopCondition = do
          isEqual <- call iterIsEqual $ (,[]) <$> [begin, end]
          not' isEqual
    loopWhile loopCondition $ do
      val <- call iterCurrent [(begin, [])]
      _ <- call btreeInsertValue $ (,[]) <$> [t, val]
      call iterNext [(begin, [])]

mkBtreeBegin :: ModuleCodegen Operand
mkBtreeBegin = do
  tree <- typeOf BTree
  iter <- typeOf Iterator

  def "btree_begin" [(ptr tree, "tree"), (ptr iter, "result")] void $ \[t, result] -> do
    assign currentPtrOf result =<< deref firstPtrOf t
    assign valuePosOf result (int16 0)

mkBtreeEnd :: Operand -> ModuleCodegen Operand
mkBtreeEnd iteratorInitEnd = do
  tree <- typeOf BTree
  iter <- typeOf Iterator

  def "btree_end" [(ptr tree, "tree"), (ptr iter, "result")] void $ \[_t, result] -> do
    _ <- call iteratorInitEnd [(result, [])]
    pure ()

mkBtreeContains :: Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeContains iterIsEqual btreeFind btreeEnd = do
  tree <- typeOf BTree
  value <- typeOf Value

  def "btree_contains" [(ptr tree, "tree"), (ptr value, "val")] i1 $ \[t, val] -> do
    iterPtr <- allocateIter
    endIterPtr <- allocateIter
    _ <- call btreeFind $ (,[]) <$> [t, val, iterPtr]
    _ <- call btreeEnd $ (,[]) <$> [t, endIterPtr]
    isEqual <- call iterIsEqual $ (,[]) <$> [iterPtr, endIterPtr]
    ret =<< not' isEqual

mkBtreeFind :: Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeFind isEmptyTree searchLowerBound compareValues iterInit iterInitEnd = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  value <- typeOf Value
  let args = [(ptr tree, "tree"), (ptr value, "val"), (ptr iter, "result")]

  def "btree_find" args void $ \[t, val, result] -> mdo
    isEmpty <- call isEmptyTree [(t, [])]
    if' isEmpty $ do
      _ <- call iterInitEnd [(result, [])]
      retVoid

    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t
    -- Find iterator using iterative approach
    loop $ mdo
      current <- load currentPtr 0
      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchLowerBound $ (,[]) <$> [val, first, last]
      idx <- pointerDiff i16 pos first

      -- Can the following equality check be done using just pointers?
      foundMatch <- pos `ult` last
      matchesVal <- (int8 0 `eq`) =<< call compareValues ((, []) <$> [pos, val])
      foundValue <- foundMatch `and` matchesVal
      if' foundValue $ do
        _ <- call iterInit $ (,[]) <$> [result, current, idx]
        retVoid

      isLeaf <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` leafNodeTypeVal)
      if' isLeaf $ do
        _ <- call iterInitEnd [(result, [])]
        retVoid

      -- Continue search in child node
      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent

mkBtreeLowerBound :: Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeLowerBound isEmptyTree iterInit iterInitEnd searchLowerBound compareValues = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  value <- typeOf Value
  let args = [(ptr tree, "tree"), (ptr value, "val"), (ptr iter, "result")]

  def "btree_lower_bound" args void $ \[t, val, result] -> mdo
    isEmpty <- call isEmptyTree [(t, [])]
    if' isEmpty $ do
      _ <- call iterInitEnd [(result, [])]
      retVoid

    res <- allocateIter
    _ <- call iterInitEnd [(res, [])]
    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t

    loop $ mdo
      current <- load currentPtr 0
      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchLowerBound $ (,[]) <$> [val, first, last]
      idx <- pointerDiff i16 pos first
      isLeaf <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` leafNodeTypeVal)
      if' isLeaf $ mdo
        isLast <- pos `eq` last
        condBr isLast handleLast handleOther

        handleLast <- block `named` "handle_last"
        copy currentPtrOf res result
        copy valuePosOf res result
        retVoid

        handleOther <- block `named` "handle_not_last"
        _ <- call iterInit $ (,[]) <$> [result, current, idx]
        retVoid

      isNotLast <- pos `ne` last
      -- Can the following be done with just pointer comparisons?
      matchesVal' <- (int8 0 `eq`) =<< call compareValues ((,[]) <$> [pos, val])
      matchFound <- isNotLast `and` matchesVal'
      if' matchFound $ do
        _ <- call iterInit $ (,[]) <$> [result, current, idx]
        retVoid

      if' isNotLast $ do
        call iterInit $ (,[]) <$> [res, current, idx]

      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent

mkBtreeUpperBound :: Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeUpperBound isEmptyTree iterInit iterInitEnd searchUpperBound = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  value <- typeOf Value
  let args = [(ptr tree, "tree"), (ptr value, "val"), (ptr iter, "result")]

  def "btree_upper_bound" args void $ \[t, val, result] -> mdo
    isEmpty <- call isEmptyTree [(t, [])]
    if' isEmpty $ do
      _ <- call iterInitEnd [(result, [])]
      retVoid

    res <- allocateIter
    _ <- call iterInitEnd [(res, [])]
    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t

    loop $ mdo
      current <- load currentPtr 0
      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchUpperBound $ (,[]) <$> [val, first, last]
      idx <- pointerDiff i16 pos first
      isLeaf <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` leafNodeTypeVal)
      if' isLeaf $ mdo
        isLast <- pos `eq` last
        condBr isLast handleLast handleOther

        handleLast <- block `named` "handle_last"
        copy currentPtrOf res result
        copy valuePosOf res result
        retVoid

        handleOther <- block `named` "handle_not_last"
        _ <- call iterInit $ (,[]) <$> [result, current, idx]
        retVoid

      -- Can the following be done with just pointer comparisons?
      isNotLast <- pos `ne` last
      if' isNotLast $ do
        call iterInit $ (,[]) <$> [result, current, idx]

      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent

mkBtreeClear :: Operand -> ModuleCodegen Operand
mkBtreeClear nodeDelete = do
  tree <- typeOf BTree
  node <- typeOf Node

  def "btree_clear" [(ptr tree, "tree")] void $ \[t] -> do
    root <- deref rootPtrOf t
    isNotNull <- root `ne` nullPtr node
    if' isNotNull $ do
      _ <- call nodeDelete [(root, [])]
      assign rootPtrOf t (nullPtr node)
      assign firstPtrOf t (nullPtr node)

mkBtreeSwap :: ModuleCodegen Operand
mkBtreeSwap = do
  tree <- typeOf BTree

  def "btree_swap" [(ptr tree, "lhs"), (ptr tree, "rhs")] void $ \[lhs, rhs] ->
    for_ [rootPtrOf, firstPtrOf] $ \path ->
      swap path lhs rhs

leafNodeTypeVal, innerNodeTypeVal :: Operand
leafNodeTypeVal = bit 0
innerNodeTypeVal = bit 1

numKeys :: Meta -> Sizes -> Word64
numKeys settings sizes =
  numKeysHelper settings nodeMetaSize valueByteSize
  where
    nodeMetaSize = nodeDataSize sizes
    valueByteSize = valueSize sizes

-- NOTE: Where possible, use the more userfriendly numKeys function
numKeysHelper :: Meta -> Word64 -> Word64 -> Word64
numKeysHelper settings nodeMetaSize valueByteSize =
  max 3 desiredNumberOfKeys
  where
    blockByteSize = blockSize settings
    valuesByteSize =
      if blockByteSize > nodeMetaSize
      then blockByteSize - nodeMetaSize
      else 0
    desiredNumberOfKeys = valuesByteSize `div` valueByteSize

numKeysAsOperand :: ModuleCodegen Operand
numKeysAsOperand = do
  (metadata, sizes) <- asks (meta &&& typeSizes)
  pure $ int16 $ toInteger $ numKeys metadata sizes


data Index
  = NodeIdx
  | InnerNodeIdx
  | MetaIdx
  | ValueIdx
  | PositionIdx
  | NumElemsIdx
  | NodeTypeIdx
  | IteratorIdx
  | TreeIdx
  | ArrayOf Index
  | PtrOf Index

metaOf :: Path 'NodeIdx 'MetaIdx
metaOf = mkPath [int32 0]

valuesOf :: Path 'NodeIdx ('ArrayOf 'ValueIdx)
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

childrenOf :: Path 'InnerNodeIdx ('ArrayOf 'NodeIdx)
childrenOf = mkPath [int32 1]

childAt :: Operand -> Path 'InnerNodeIdx ('PtrOf 'NodeIdx)
childAt idx = mkPath [int32 1, idx]

currentPtrOf :: Path 'IteratorIdx ('PtrOf 'NodeIdx)
currentPtrOf = mkPath [int32 0]

valuePosOf :: Path 'IteratorIdx 'PositionIdx
valuePosOf = mkPath [int32 1]

rootPtrOf :: Path 'TreeIdx ('PtrOf 'NodeIdx)
rootPtrOf = mkPath [int32 0]

firstPtrOf :: Path 'TreeIdx ('PtrOf 'NodeIdx)
firstPtrOf = mkPath [int32 1]

data DataType
  = NodeType
  | Node
  | InnerNode
  | Value
  | NodeSize
  | Iterator
  | BTree

typeOf :: MonadReader CGState m => DataType -> m Type
typeOf dt =
  let getType = case dt of
        Node -> nodeTy
        NodeType -> nodeTypeTy
        InnerNode -> innerNodeTy
        Value -> valueTy
        NodeSize -> nodeSizeTy
        Iterator -> iteratorTy
        BTree -> btreeTy
   in getType <$> asks types

memset :: Operand -> Word8 -> Word64 -> IRCodegen ()
memset p val byteCount = do
  memsetFn <- asks (extMemset . externals)
  p' <- p `bitcast` ptr i8
  _ <- call memsetFn [ (p', [])
                     , (int8 $ fromIntegral val, [])
                     , (int64 (fromIntegral byteCount), [])
                     , (bit 0, [])
                     ]
  pure ()

-- NOTE: this only allocates on stack, but doesn't initialize it,
-- this still needs to happen in rest of the code
allocateIter :: IRCodegen Operand
allocateIter = do
  iter <- typeOf Iterator
  alloca iter (Just (int32 1)) 0
