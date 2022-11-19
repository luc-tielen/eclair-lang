{-# LANGUAGE RecursiveDo, FlexibleContexts, ScopedTypeVariables #-}

module Eclair.LLVM.BTree
  ( Meta(..)
  , SearchIndex
  , SearchType(..)
  , codegen
  ) where

import Prelude hiding (void)
import Control.Monad.Morph
import qualified Data.Map as Map
import Eclair.LLVM.Codegen
import Eclair.LLVM.Table
import Eclair.LLVM.Externals
import Eclair.LLVM.Hash
import Prettyprinter


data Meta
  = Meta
  { numColumns :: Int        -- Amount of columns each node has
  , index :: SearchIndex     -- Which columns are used to index values
  , blockSize :: Word64      -- Number of bytes per btree node
  , searchType :: SearchType -- Search strategy used in a single node
  }
  deriving stock (Eq, Ord, Show)
  deriving stock Generic
  deriving ToHash via HashWithPrefix "btree" Meta

instance Pretty Meta where
  pretty meta =
    "num_columns=" <> pretty (numColumns meta) <> comma <+>
    -- TODO: use "withCommas"
    "index=" <> brackets (Prelude.fold $ intersperse comma $ map pretty (index meta)) <> comma <+>
    "block_size=" <> pretty (blockSize meta) <> comma <+>
    "search_type=" <> pretty (searchType meta)

type Column = Int

type SearchIndex = [Column]

data SearchType = Linear | Binary
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic, Enum)

instance ToHash SearchType where
  getHash = \case
    Linear -> getHash ("linear" :: Text)
    Binary -> getHash ("binary" :: Text)

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

data Sizes
  = Sizes
  { pointerSize :: Word64
  , valueSize :: Word64
  , nodeDataSize :: Word64
  , leafNodeSize :: Word64
  , innerNodeSize :: Word64
  }

-- State used during rest of the btree codegen
data CGState
  = CGState
  { types :: Types
  , typeSizes :: Sizes
  , externals :: Externals
  }

type IRCodegen = IRBuilderT ModuleCodegen

type ModuleCodegen = ReaderT CGState (Template Meta)


codegen :: Externals -> ConfigT (TemplateT Meta IO) Table
codegen exts = do
  sizes <- computeSizes
  lift $ hoist intoIO $ do
    tys <- generateTypes sizes
    runReaderT generateTableFunctions $ CGState tys sizes exts
  where intoIO = pure . runIdentity

-- TODO: can be merged with generateTypes now with llvm-codegen?
computeSizes :: ConfigT (TemplateT Meta IO) Sizes
computeSizes = do
  (ctx, td) <- (cfgLLVMContext &&& cfgTargetData) <$> getConfig
  settings <- getParams
  let nodeDataTy = StructureType Off
        [ -- Next type doesn't matter here, but we need to break the
          -- cyclic loop or Haskell will throw an exception.
          ptrTy   -- parent
        , i16     -- position_in_parent
        , i16     -- num_elements
        , i1      -- node type
        ]
      ptrTy = ptr i8
      valueType = ArrayType (fromIntegral $ numColumns settings) i32
  (ptrSz, valueSz, nodeDataSz) <- withLLVMTypeInfo ctx $ do
    let sizeOf = llvmSizeOf ctx td
    pointerSize' <- sizeOf ptrTy
    valueSize' <- sizeOf valueType
    nodeDataSize' <- sizeOf nodeDataTy
    pure (pointerSize', valueSize', nodeDataSize')

  let numKeys' = fromIntegral $ numKeysHelper settings nodeDataSz valueSz
      nodeType = StructureType Off [nodeDataTy, ArrayType numKeys' valueType]
      innerNodeType = StructureType Off [nodeType, ArrayType (numKeys' + 1) (ptr nodeType)]
  (leafNodeSz, innerNodeSz) <- withLLVMTypeInfo ctx $ do
    let sizeOf = llvmSizeOf ctx td
    leafNodeSize' <- sizeOf nodeType
    innerNodeSize' <- sizeOf innerNodeType
    pure (leafNodeSize', innerNodeSize')

  pure $ Sizes ptrSz valueSz nodeDataSz leafNodeSz innerNodeSz

generateTypes :: (MonadModuleBuilder m, MonadFix m, MonadTemplate Meta m, HasSuffix m)
              => Sizes -> m Types
generateTypes sizes = mdo
  meta <- getParams
  let numKeys' = fromIntegral $ numKeys meta sizes

  let columnTy' = i32
      valueTy' = ArrayType (fromIntegral $ numColumns meta) columnTy'
      positionTy = i16
      nodeSizeTy' = i16  -- Note: used to be size_t/i64
      nodeTypeTy' = i1
      nodeDataName = "node_data_t"
  nodeDataTy <- typedef nodeDataName Off
    [ ptr nodeTy  -- parent
    , positionTy  -- position_in_parent
    , nodeSizeTy'  -- num_elements
    , nodeTypeTy'  -- node type
    ]
  nodeTy <- typedef "node_t" Off
    [ nodeDataTy                  -- meta
    , ArrayType numKeys' valueTy'  -- values
    ]
  let leafNodeTy' = nodeTy
  innerNodeTy <- typedef "inner_node_t" Off
    [ nodeTy                                 -- base
    , ArrayType (numKeys' + 1) (ptr nodeTy)  -- children
    ]
  btreeIteratorTy <- typedef "btree_iterator_t" Off
    [ ptr nodeTy  -- current
    , positionTy  -- value pos
    ]
  btreeTy <- typedef "btree_t" Off
    [ ptr nodeTy  -- root
    , ptr nodeTy  -- first
    ]
  pure $ Types
    { btreeTy = btreeTy
    , iteratorTy = btreeIteratorTy
    , nodeSizeTy = nodeSizeTy'
    , nodeTypeTy = nodeTypeTy'
    , nodeTy = nodeTy
    , leafNodeTy = leafNodeTy'
    , innerNodeTy = innerNodeTy
    , valueTy = valueTy'
    , columnTy = columnTy'
    }

generateTableFunctions :: ModuleCodegen Table
generateTableFunctions = mdo
  tree <- typeOf BTree
  iter <- typeOf Iterator
  value <- typeOf Value

  compareValues <- mkCompare
  nodeNew <- mkNodeNew
  nodeDelete <- mkNodeDelete
  nodeCountEntries <- mkNodeCountEntries
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
  btreeSize <- mkBtreeSize nodeCountEntries
  btreeInsert <- mkBtreeInsertValue nodeNew rebalanceOrSplit compareValues searchLowerBound searchUpperBound isEmptyTree
  btreeInsertRangeTemplate <- mkBtreeInsertRangeTemplate btreeInsert
  -- We need to instantiate it atleast once for use in the BTree itself.
  let iterParams = IteratorParams
        { ipIterCurrent = iterCurrent
        , ipIterNext = iterNext
        , ipIterIsEqual = iterIsEqual
        , ipTypeIter = iter
        }
  btreeInsertRange <- lift $ partialInstantiate iterParams btreeInsertRangeTemplate
  btreeBegin <- mkBtreeBegin
  btreeEnd <- mkBtreeEnd iterInitEnd
  btreeContains <- mkBtreeContains iterIsEqual btreeFind btreeEnd
  btreeFind <- mkBtreeFind isEmptyTree searchLowerBound compareValues iterInit iterInitEnd
  btreeLowerBound <- mkBtreeLowerBound isEmptyTree iterInit iterInitEnd searchLowerBound compareValues
  btreeUpperBound <- mkBtreeUpperBound isEmptyTree iterInit iterInitEnd searchUpperBound
  btreeClear <- mkBtreeClear nodeDelete
  btreeSwap <- mkBtreeSwap

  pure Table
        { fnInit = btreeInit
        , fnInitEmpty = btreeInitEmpty
        , fnDestroy = btreeDestroy
        , fnPurge = btreeClear
        , fnSwap = btreeSwap
        , fnBegin = btreeBegin
        , fnEnd = btreeEnd
        , fnInsert = btreeInsert
        , fnInsertRangeTemplate = btreeInsertRangeTemplate
        , fnIsEmpty = isEmptyTree
        , fnSize = btreeSize
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
  settings <- getParams
  tys <- asks types
  let column' = columnTy tys
      value = valueTy tys
  compare' <- function "btree_value_compare" [(column', "lhs"), (column', "rhs")] i8 $ \[lhs, rhs] -> mdo
    result1 <- lhs `ult` rhs
    if' result1 $
      ret $ int8 (-1)
    result2 <- lhs `ugt` rhs
    ret =<< select result2 (int8 1) (int8 0)

  function "btree_value_compare_values" [(ptr value, "lhs"), (ptr value, "rhs")] i8 $ \[lhs, rhs] -> mdo
    let columns = map fromIntegral $ index settings
    results <- flip execStateT mempty $ flip (zygo endCheck) columns $ \case
      Nil -> pass
      Cons col (atEnd, asm) -> do
        blk <- block `named` "comparison"
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
    end <- block `named` "end"
    ret =<< phi (Map.toList results)
  where
    endCheck = \case
      Nil -> End
      _ -> Continue

data ControlFlow = Continue | End

mkNodeNew :: ModuleCodegen Operand
mkNodeNew = mdo
  md <- getParams
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

  function "btree_node_new" [(nodeType, "type")] (ptr node) $ \[ty] -> mdo
    structSize <- select ty leafSize innerSize
    memory <- call malloc [structSize]
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

  nodeDelete <- function "btree_node_delete" [(ptr node, "node")] void $ \[n] -> mdo
    nodeTy <- deref (metaOf ->> nodeTypeOf) n
    isInner <- nodeTy `eq` innerNodeTypeVal
    if' isInner $ do  -- Delete children of inner node
      inner <- n `bitcast` ptr innerNode

      numElements <- deref (metaOf ->> numElemsOf) n
      loopFor (int16 0) (`ule` numElements) (add (int16 1)) $ \i -> mdo
        child <- deref (childAt i) inner
        isNotNull <- child `ne` nullPtr node
        if' isNotNull $
          call nodeDelete [child]

    memory <- n `bitcast` ptr i8
    _ <- call free [memory]
    pass

  pure nodeDelete

mkNodeCountEntries :: ModuleCodegen Operand
mkNodeCountEntries = mdo
  node <- typeOf Node

  countEntries <- function "node_count_entries" [(ptr node, "node")] i64 $ \[n] -> mdo
    numElements <- deref (metaOf ->> numElemsOf) n
    ty <- deref (metaOf ->> nodeTypeOf) n
    isLeaf <- ty `eq` leafNodeTypeVal
    numElements' <- zext numElements i64
    if' isLeaf $
      ret numElements'

    count <- loopChildren n i64 numElements' $ \entryCount child -> mdo
      childNodeCount <- call countEntries [child]
      add entryCount childNodeCount
    ret count

  pure countEntries
  where
    loopChildren n ty beginValue f = mdo
      innerNode <- typeOf InnerNode
      inner <- n `bitcast` ptr innerNode

      result <- allocate ty beginValue
      numElements <- deref (metaOf ->> numElemsOf) n
      loopFor (int16 0) (`ule` numElements) (add (int16 1)) $ \i -> mdo
        currentResult <- load result 0
        child <- deref (childAt i) inner
        updatedResult <- f currentResult child
        store result 0 updatedResult

      load result 0

mkNodeSplitPoint :: ModuleCodegen Operand
mkNodeSplitPoint = mdo
  nodeSize <- typeOf NodeSize
  numberOfKeys <- numKeysAsOperand

  function "btree_node_split_point" [] nodeSize $ \_ -> mdo
    a' <- mul (int16 3) numberOfKeys
    a <- udiv a' (int16 4)
    b <- sub numberOfKeys (int16 2)
    ret =<< minimum' Unsigned a b

mkSplit :: Operand -> Operand -> Operand -> ModuleCodegen Operand
mkSplit nodeNew nodeSplitPoint growParent = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  numberOfKeys <- numKeysAsOperand

  function "btree_node_split" [(ptr node, "node"), (ptr (ptr node), "root")] void $ \[n, root] -> mdo
    -- TODO: how to do assertions in LLVM?
    -- assert(n->meta.num_elements == NUM_KEYS);
    splitPoint <- call nodeSplitPoint []
    splitPoint' <- add (int16 1) splitPoint
    ty <- deref (metaOf ->> nodeTypeOf) n
    -- Create a new sibling node and move some of the data to sibling
    sibling <- call nodeNew [ty]
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

    _ <- call growParent [n, root, sibling]
    pass

mkGrowParent :: Operand -> Operand -> ModuleCodegen Operand
mkGrowParent nodeNew insertInner = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode

  function "btree_node_grow_parent" [(ptr node, "node"), (ptr (ptr node), "root"), (ptr node, "sibling")] void $
    \[n, root, sibling] -> mdo
    parent <- deref (metaOf ->> parentOf) n
    isNull <- parent `eq` nullPtr node
    numElems <- deref (metaOf ->> numElemsOf) n
    condBr isNull createNewRoot insertNewNodeInParent

    createNewRoot <- block `named` "create_new_root"
    -- TODO: assert(n == *root)
    newRoot <- call nodeNew [innerNodeTypeVal]
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
    _ <- call insertInner [parent, root, pos, n, lastValuePtr, sibling]
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

  insertInner <- function "btree_node_insert_inner" args void $
    \[n, root, pos, predecessor, key, newNode] -> mdo
    -- Need to allocate pos on the stack, otherwise pos updates are
    -- not visible later on!
    posPtr <- allocate nodeSize pos

    numElems <- deref (metaOf ->> numElemsOf) n
    needsRebalanceOrSplit <- numElems `uge` numberOfKeys

    if' needsRebalanceOrSplit $ do
      position' <- load posPtr 0
      position'' <- sub position' =<< call rebalanceOrSplit [n, root, pos]
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
        _ <- call insertInner [sibling, root, pos''', predecessor, key, newNode]
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
  function "btree_node_rebalance_or_split" args nodeSize $ \[n, root, idx] -> mdo
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
    _ <- call splitFn [n, root]
    ret (int16 0)  -- No re-balancing
  where
    calculateLeftSlotsOpen numberOfKeys left' idx = do
      numElems <- deref (metaOf ->> numElemsOf) left'
      openSlots <- sub numberOfKeys numElems
      isLessThan <- openSlots `slt` idx
      select isLessThan openSlots idx

mkIteratorInit :: ModuleCodegen Operand
mkIteratorInit = do
  iter <- typeOf Iterator
  node <- typeOf Node
  nodeSize <- typeOf NodeSize
  let args = [(ptr iter, "iter"), (ptr node, "cur"), (nodeSize, "pos")]

  function "btree_iterator_init" args void $ \[it, cur, pos] -> do
    assign currentPtrOf it cur
    assign valuePosOf it pos

mkIteratorInitEnd :: Operand -> ModuleCodegen Operand
mkIteratorInitEnd iterInit = do
  iter <- typeOf Iterator
  node <- typeOf Node

  function "btree_iterator_end_init" [(ptr iter, "iter")] void $ \[it] -> do
    _ <- call iterInit [it, nullPtr node, int16 0]
    retVoid

mkIteratorIsEqual :: ModuleCodegen Operand
mkIteratorIsEqual = do
  iter <- typeOf Iterator

  function "btree_iterator_is_equal" [(ptr iter, "lhs"), (ptr iter, "rhs")] i1 $ \[lhs, rhs] -> mdo
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

  function "btree_iterator_current" [(ptr iter, "iter")] (ptr value) $ \[it] -> mdo
    valuePos <- deref valuePosOf it
    currentNode <- deref currentPtrOf it
    ret =<< addr (valueAt valuePos) currentNode

mkIteratorNext :: ModuleCodegen Operand
mkIteratorNext = do
  iter <- typeOf Iterator

  function "btree_iterator_next" [(ptr iter, "iter")] void $ \[it] -> mdo
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
      let loopCondition = mdo
            isNull <- deref currentPtrOf iter >>= (`eq` nullPtr node)
            condBr isNull nullBlock notNullBlock
            nullBlock <- block `named` "leaf.no_parent"
            br endLoopCondition

            notNullBlock <- block `named` "leaf.has_parent"
            pos' <- deref valuePosOf iter
            current' <- deref currentPtrOf iter
            numElems' <- deref (metaOf ->> numElemsOf) current'
            atEnd <- pos' `eq` numElems'

            br endLoopCondition

            endLoopCondition <- block `named` "loop.condition.end"
            phi [(bit 0, nullBlock), (atEnd, notNullBlock)]
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

  function "btree_linear_search_lower_bound" args (ptr value) $ \[val, curr, end] -> mdo
    -- Finds an iterator to first element not less than given value.
    currentPtr <- allocate (ptr value) curr
    let loopCondition = do
          current <- load currentPtr 0
          current `ne` end
    loopWhile loopCondition $ mdo
      current <- load currentPtr 0
      result <- call compareValues [current, val]
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

  function "btree_linear_search_upper_bound" args (ptr value) $ \[val, curr, end] -> mdo
    -- Finds an iterator to first element that is greater than given value.
    currentPtr <- allocate (ptr value) curr
    let loopCondition = do
          current <- load currentPtr 0
          current `ne` end
    loopWhile loopCondition $ mdo
      current <- load currentPtr 0
      result <- call compareValues [current, val]
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

  function "btree_init_empty" [(ptr tree, "tree")] void $ \[t] -> mdo
    assign rootPtrOf t (nullPtr node)
    assign firstPtrOf t (nullPtr node)

mkBtreeInit :: Operand -> ModuleCodegen Operand
mkBtreeInit btreeInsertRange = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  let args = [(ptr tree, "tree"), (ptr iter, "start"), (ptr iter, "end")]

  function "btree_init" args void $ \[t, start, end] -> mdo
    _ <- call btreeInsertRange [t, start, end]
    pass

mkBtreeDestroy :: Operand -> ModuleCodegen Operand
mkBtreeDestroy btreeClear = do
  tree <- typeOf BTree

  function "btree_destroy" [(ptr tree, "tree")] void $ \[t] -> do
    _ <- call btreeClear [t]
    pass

mkBtreeIsEmpty :: ModuleCodegen Operand
mkBtreeIsEmpty = do
  tree <- typeOf BTree
  node <- typeOf Node

  function "btree_is_empty" [(ptr tree, "tree")] i1 $ \[t] -> do
    root <- deref rootPtrOf t
    ret =<< root `eq` nullPtr node

mkBtreeSize :: Operand -> ModuleCodegen Operand
mkBtreeSize nodeCountEntries = do
  tree <- typeOf BTree
  node <- typeOf Node

  function "btree_size" [(ptr tree, "tree")] i64 $ \[t] -> mdo
    root <- deref rootPtrOf t
    isNull <- root `eq` nullPtr node
    condBr isNull nullBlock notNullBlock

    nullBlock <- block `named` "null"
    ret (int64 0)

    notNullBlock <- block `named` "not_null"
    count <- call nodeCountEntries [root]
    ret count

mkBtreeInsertValue :: Operand -> Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeInsertValue nodeNew rebalanceOrSplit compareValues searchLowerBound searchUpperBound isEmptyTree = do
  tree <- typeOf BTree
  node <- typeOf Node
  value <- typeOf Value
  numberOfKeys <- numKeysAsOperand

  function "btree_insert_value" [(ptr tree, "tree"), (ptr value, "val")] i1 $ \[t, val] -> mdo
    isEmpty <- call isEmptyTree [t]
    condBr isEmpty emptyCase nonEmptyCase

    emptyCase <- block `named` "empty"
    leaf <- call nodeNew [leafNodeTypeVal]
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
      valSize <- asks (valueSize . typeSizes)

      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchLowerBound [val, first, last]
      idx <- pointerDiff i16 pos first >>= (`udiv` int32 (toInteger valSize))
      notLast <- pos `ne` last
      valueAtPos <- gep pos [int32 0]
      isEqual <- (int8 0 `eq`) =<< call compareValues [valueAtPos, val]  -- Can we do a weak compare just by using pointers here?
      alreadyInserted <- notLast `and` isEqual
      condBr alreadyInserted noInsert continueInsert

      continueInsert <- block `named` "inner_continue_insert"
      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent
      br loopBlock

    insertInNonEmptyLeafNode noInsert inserted t currentPtr current val numberOfKeys = mdo
      -- Rest is for leaf nodes
      innerNode <- typeOf InnerNode
      valSize <- asks (valueSize . typeSizes)

      -- TODO: assert(current->meta.type == LEAF_NODE);
      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchUpperBound [val, first, last]
      distance <- pointerDiff i16 pos first >>= (`udiv` int32 (toInteger valSize))
      idxPtr <- allocate i16 distance
      notFirst <- pos `ne` first
      valueAtPrevPos <- gep pos [int32 (-1)]
      isEqual <- (int8 0 `eq`) =<< call compareValues [valueAtPrevPos, val]  -- Can we do a weak compare just by using pointers here?
      alreadyInserted <- notFirst `and` isEqual
      condBr alreadyInserted noInsert continueInsert

      continueInsert <- block `named` "leaf_continue_insert"
      nodeIsFull <- numElems `uge` numberOfKeys
      condBr nodeIsFull split noSplit

      split <- block `named` "split"
      root <- addr rootPtrOf t
      idx <- load idxPtr 0
      res <- call rebalanceOrSplit [current, root, idx]
      idx' <- sub idx res
      store idxPtr 0 idx'

      -- Insert in right fragment if needed
      numElems' <- deref (metaOf ->> numElemsOf) current  -- NOTE: numElems' modified after rebalanceOrSplit
      shouldInsertRight <- idx' `ugt` numElems'
      if' shouldInsertRight $ do
        numElems'' <- add numElems' (int16 1)
        idx'' <- sub idx' numElems''
        store idxPtr 0 idx''
        parent <- deref (metaOf ->> parentOf) current >>= (`bitcast` ptr innerNode)
        nextPos <- deref (metaOf ->> posInParentOf) current >>= add (int16 1)
        store currentPtr 0 =<< deref (childAt nextPos) parent

      br noSplit

      noSplit <- block `named` "no_split"
      -- No split -> move keys and insert new element
      idx''' <- load idxPtr 0
      numElems''' <- deref (metaOf ->> numElemsOf) current  -- NOTE: Might've been updated in the meantime
      loopFor numElems''' (`ugt` idx''') (`sub` int16 1) $ \j -> do
        -- TODO: memmove possible?
        j' <- sub j (int16 1)
        assign (valueAt j) current =<< deref (valueAt j') current

      assign (valueAt idx''') current =<< load val 0
      update (metaOf ->> numElemsOf) current (add (int16 1))
      br inserted

mkBtreeInsertRangeTemplate :: Operand -> ModuleCodegen (Template IteratorParams Operand)
mkBtreeInsertRangeTemplate btreeInsertValue = do
  -- Context of BTree template
  tree <- typeOf BTree
  pure $ do
    -- Context of insert range template
    iterParams <- getParams
    let iterTy = ipTypeIter iterParams
        args = [(ptr tree, "tree"), (ptr iterTy, "begin"), (ptr iterTy, "end")]
    function "btree_insert_range" args void $ \[t, begin, end] -> do
      let loopCondition = do
            isEqual <- call (ipIterIsEqual iterParams) [begin, end]
            not' isEqual
      loopWhile loopCondition $ do
        -- NOTE: Can directly insert value in other btree, same array type!
        val <- call (ipIterCurrent iterParams) [begin]
        _ <- call btreeInsertValue [t, val]
        call (ipIterNext iterParams) [begin]

mkBtreeBegin :: ModuleCodegen Operand
mkBtreeBegin = do
  tree <- typeOf BTree
  iter <- typeOf Iterator

  function "btree_begin" [(ptr tree, "tree"), (ptr iter, "result")] void $ \[t, result] -> do
    assign currentPtrOf result =<< deref firstPtrOf t
    assign valuePosOf result (int16 0)

mkBtreeEnd :: Operand -> ModuleCodegen Operand
mkBtreeEnd iteratorInitEnd = do
  tree <- typeOf BTree
  iter <- typeOf Iterator

  function "btree_end" [(ptr tree, "tree"), (ptr iter, "result")] void $ \[_t, result] -> do
    _ <- call iteratorInitEnd [result]
    pass

mkBtreeContains :: Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeContains iterIsEqual btreeFind btreeEnd = do
  tree <- typeOf BTree
  value <- typeOf Value

  function "btree_contains" [(ptr tree, "tree"), (ptr value, "val")] i1 $ \[t, val] -> do
    iterPtr <- allocateIter
    endIterPtr <- allocateIter
    _ <- call btreeFind [t, val, iterPtr]
    _ <- call btreeEnd [t, endIterPtr]
    isEqual <- call iterIsEqual [iterPtr, endIterPtr]
    ret =<< not' isEqual

mkBtreeFind :: Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeFind isEmptyTree searchLowerBound compareValues iterInit iterInitEnd = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  value <- typeOf Value
  valSize <- asks (valueSize . typeSizes)

  let args = [(ptr tree, "tree"), (ptr value, "val"), (ptr iter, "result")]

  function "btree_find" args void $ \[t, val, result] -> mdo
    isEmpty <- call isEmptyTree [t]
    if' isEmpty $ do
      _ <- call iterInitEnd [result]
      retVoid

    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t
    -- Find iterator using iterative approach
    loop $ mdo
      current <- load currentPtr 0
      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchLowerBound [val, first, last]
      idx <- pointerDiff i16 pos first >>= (`udiv` int32 (toInteger valSize))

      -- Can the following equality check be done using just pointers?
      foundMatch <- pos `ult` last
      matchesVal <- (int8 0 `eq`) =<< call compareValues [pos, val]
      foundValue <- foundMatch `and` matchesVal
      if' foundValue $ do
        _ <- call iterInit [result, current, idx]
        retVoid

      isLeaf <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` leafNodeTypeVal)
      if' isLeaf $ do
        _ <- call iterInitEnd [result]
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
  valSize <- asks (valueSize . typeSizes)
  let args = [(ptr tree, "tree"), (ptr value, "val"), (ptr iter, "result")]

  function "btree_lower_bound" args void $ \[t, val, result] -> mdo
    isEmpty <- call isEmptyTree [t]
    if' isEmpty $ do
      _ <- call iterInitEnd [result]
      retVoid

    res <- allocateIter
    _ <- call iterInitEnd [res]
    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t

    loop $ mdo
      current <- load currentPtr 0
      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchLowerBound [val, first, last]
      idx <- pointerDiff i16 pos first >>= (`udiv` int32 (toInteger valSize))
      isLeaf <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` leafNodeTypeVal)
      if' isLeaf $ mdo
        isLast <- pos `eq` last
        condBr isLast handleLast handleOther

        handleLast <- block `named` "handle_last"
        copy currentPtrOf res result
        copy valuePosOf res result
        retVoid

        handleOther <- block `named` "handle_not_last"
        _ <- call iterInit [result, current, idx]
        retVoid

      isNotLast <- pos `ne` last
      -- Can the following be done with just pointer comparisons?
      matchesVal' <- (int8 0 `eq`) =<< call compareValues [pos, val]
      matchFound <- isNotLast `and` matchesVal'
      if' matchFound $ do
        _ <- call iterInit [result, current, idx]
        retVoid

      if' isNotLast $ do
        call iterInit [res, current, idx]

      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent

mkBtreeUpperBound :: Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeUpperBound isEmptyTree iterInit iterInitEnd searchUpperBound = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  value <- typeOf Value
  valSize <- asks (valueSize . typeSizes)
  let args = [(ptr tree, "tree"), (ptr value, "val"), (ptr iter, "result")]

  function "btree_upper_bound" args void $ \[t, val, result] -> mdo
    isEmpty <- call isEmptyTree [t]
    if' isEmpty $ do
      _ <- call iterInitEnd [result]
      retVoid

    res <- allocateIter
    _ <- call iterInitEnd [res]
    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t

    loop $ mdo
      current <- load currentPtr 0
      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchUpperBound [val, first, last]
      idx <- pointerDiff i16 pos first >>= (`udiv` int32 (toInteger valSize))
      isLeaf <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` leafNodeTypeVal)
      if' isLeaf $ mdo
        isLast <- pos `eq` last
        condBr isLast handleLast handleOther

        handleLast <- block `named` "handle_last"
        copy currentPtrOf res result
        copy valuePosOf res result
        retVoid

        handleOther <- block `named` "handle_not_last"
        _ <- call iterInit [result, current, idx]
        retVoid

      -- Can the following be done with just pointer comparisons?
      isNotLast <- pos `ne` last
      if' isNotLast $ do
        call iterInit [result, current, idx]

      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent

mkBtreeClear :: Operand -> ModuleCodegen Operand
mkBtreeClear nodeDelete = do
  tree <- typeOf BTree
  node <- typeOf Node

  function "btree_clear" [(ptr tree, "tree")] void $ \[t] -> do
    root <- deref rootPtrOf t
    isNotNull <- root `ne` nullPtr node
    if' isNotNull $ do
      _ <- call nodeDelete [root]
      assign rootPtrOf t (nullPtr node)
      assign firstPtrOf t (nullPtr node)

mkBtreeSwap :: ModuleCodegen Operand
mkBtreeSwap = do
  tree <- typeOf BTree

  function "btree_swap" [(ptr tree, "lhs"), (ptr tree, "rhs")] void $ \[lhs, rhs] ->
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
  metadata <- getParams
  sizes <- asks typeSizes
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
  _ <- call memsetFn [ p'
                     , int8 $ fromIntegral val
                     , int64 (fromIntegral byteCount)
                     , bit 0
                     ]
  pass

-- NOTE: this only allocates on stack, but doesn't initialize it,
-- this still needs to happen in rest of the code
allocateIter :: IRCodegen Operand
allocateIter = do
  iter <- typeOf Iterator
  alloca iter (Just (int32 1)) 0
