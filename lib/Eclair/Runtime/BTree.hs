{-# LANGUAGE RecursiveDo, FlexibleContexts, ScopedTypeVariables #-}

module Eclair.Runtime.BTree
  ( Meta(..)
  , Architecture(..)
  , SearchIndex
  , SearchType(..)
  , codegen
  ) where

import Protolude hiding ( Type, Meta, swap, void, bit, typeOf, minimum, and, not )
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
generateFunctions = mdo
  compareValues <- mkCompare
  nodeNew <- mkNodeNew
  nodeDelete <- mkNodeDelete
  nodeClone <- mkNodeClone nodeNew
  nodeDepth <- mkNodeDepth
  mkNodeCount
  nodeCountEntries <- mkNodeCountEntries
  mkNodeIsEmpty
  mkNodeIsFull
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
  mkBtreeInitEmpty
  mkBtreeInit btreeInsert
  mkBtreeCopy nodeClone isEmptyTree
  mkBtreeDestroy btreeClear
  isEmptyTree <- mkBtreeIsEmpty
  btreeSize <- mkBtreeSize nodeCountEntries
  btreeInsert <- mkBtreeInsertValue nodeNew rebalanceOrSplit compareValues searchLowerBound searchUpperBound isEmptyTree
  btreeInsertRange <- mkBtreeInsertRange iterIsEqual iterCurrent iterNext btreeInsert
  btreeBegin <- mkBtreeBegin
  btreeEnd <- mkBtreeEnd
  btreeContains <- mkBtreeContains iterIsEqual btreeFind btreeEnd
  btreeFind <- mkBtreeFind btreeEnd isEmptyTree searchLowerBound compareValues iterInit iterInitEnd
  mkBtreeLowerBound isEmptyTree iterInit iterInitEnd searchLowerBound compareValues
  mkBtreeUpperBound isEmptyTree iterInit iterInitEnd searchUpperBound compareValues
  btreeClear <- mkBtreeClear nodeDelete
  mkBtreeSwap
  mkBtreeIsEqual btreeBegin btreeEnd btreeContains btreeSize iterIsEqual iterNext iterCurrent
  mkBtreeAssign isEmptyTree nodeClone
  mkBtreeDepth isEmptyTree nodeDepth
  mkBtreeCountNodes isEmptyTree nodeCountEntries
  pure ()

mkCompare :: ModuleCodegen Operand
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

mkNodeClone :: Operand -> ModuleCodegen Operand
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
  pure nodeClone
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

mkNodeDepth :: ModuleCodegen Operand
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

  pure nodeDepth

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

mkNodeCountEntries :: ModuleCodegen Operand
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

  pure countEntries

mkNodeIsEmpty :: ModuleCodegen ()
mkNodeIsEmpty = mdo
  node <- typeOf Node

  function "node_is_empty" [(ptr node, "node")] i1 $ \[n] -> mdo
    numElements <- deref (metaOf ->> numElemsOf) n
    ret =<< numElements `eq` int16 0

  pure ()

mkNodeIsFull :: ModuleCodegen ()
mkNodeIsFull = mdo
  numberOfKeys <- numKeysAsOperand
  node <- typeOf Node

  function "node_is_full" [(ptr node, "node")] i1 $ \[n] -> mdo
    numElements <- deref (metaOf ->> numElemsOf) n
    ret =<< numElements `eq` numberOfKeys

  pure ()

mkNodeSplitPoint :: ModuleCodegen Operand
mkNodeSplitPoint = mdo
  nodeSize <- typeOf NodeSize
  numberOfKeys <- numKeysAsOperand

  function "node_split_point" [] nodeSize $ \_ -> mdo
    a' <- mul (int16 3) numberOfKeys
    a <- udiv a' (int16 4)
    b <- sub numberOfKeys (int16 2)
    ret =<< minimum a b

mkSplit :: Operand -> Operand -> Operand -> ModuleCodegen Operand
mkSplit nodeNew nodeSplitPoint growParent = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  numberOfKeys <- numKeysAsOperand

  function "node_split" [(ptr node, "node"), (ptr (ptr node), "root")] void $ \[n, root] -> mdo
    -- TODO: how to do assertions in LLVM?
    -- assert(n->meta.num_elements == NUM_KEYS);
    splitPoint <- call nodeSplitPoint []
    splitPoint' <- add (int16 1) splitPoint
    ty <- deref (metaOf ->> nodeTypeOf) n
    -- Create a new sibling node and move some of the data to sibling
    sibling <- call nodeNew [(ty, [])]
    jPtr <- allocate i16 (int16 0)
    forLoop splitPoint' (`ult` numberOfKeys) (add (int16 1)) $ \i -> mdo
      j <- load jPtr 0
      siblingValue <- deref (valueAt j) sibling
      assign (valueAt i) n siblingValue
      j' <- add (int16 1) j
      store jPtr 0 j'

    isInner <- ty `eq` bit 1
    if' isInner $ mdo
      iSibling <- sibling `bitcast` ptr innerNode
      iNode <- n `bitcast` ptr innerNode

      store jPtr 0 (int16 0)
      forLoop splitPoint' (`ult` numberOfKeys) (add (int16 1)) $ \i -> mdo
        j <- load jPtr 0
        -- TODO: check if this is correctly implemented
        iChild <- deref (childAt i) iNode
        assign (metaOf ->> parentOf) iChild sibling
        assign (metaOf ->> numElemsOf) iChild j
        assign (childAt j) iSibling iChild
        j' <- add (int16 1) j
        store jPtr 0 j'

    assign (metaOf ->> numElemsOf) n splitPoint
    siblingNumKeys <- sub numberOfKeys splitPoint >>= flip sub (int16 1)
    assign (metaOf ->> numElemsOf) sibling siblingNumKeys

    call growParent [(n, []), (root, []), (sibling, [])]
    pure ()

mkGrowParent :: Operand -> Operand -> ModuleCodegen Operand
mkGrowParent nodeNew insertInner = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode

  function "node_grow_parent" [(ptr node, "node"), (ptr (ptr node), "root"), (ptr node, "sibling")] void $
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
    call insertInner $ (, []) <$> [parent, root, pos, n, lastValuePtr, sibling]
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

  insertInner <- function "node_insert_inner" args void $
    \[n, root, pos, predecessor, key, newNode] -> mdo
    -- Need to allocate pos on the stack, otherwise pos updates are
    -- not visible later on!
    posPtr <- allocate nodeSize pos

    numElems <- deref (metaOf ->> numElemsOf) n
    needsRebalanceOrSplit <- numElems `uge` numberOfKeys

    if' needsRebalanceOrSplit $ do
      pos' <- load posPtr 0
      pos'' <- sub pos' <=< call rebalanceOrSplit $ (,[]) <$> [n, root, pos]
      store posPtr 0 pos''
      numElems' <- deref (metaOf ->> numElemsOf) n
      needsInsertInNewNode <- pos'' `ugt` numElems'

      if' needsInsertInNewNode $ do
        -- Insertion needs to be done in new sibling node:
        pos''' <- sub pos'' numElems' >>= flip sub (int16 1)
        store posPtr 0 pos'''
        parent <- deref (metaOf ->> parentOf) n >>= (`bitcast` ptr innerNode)
        siblingPos <- add (int16 1) =<< deref (metaOf ->> posInParentOf) n
        sibling <- deref (childAt siblingPos) parent
        call insertInner $ (, []) <$> [sibling, root, pos''', predecessor, key, newNode]
        retVoid

    -- Move bigger keys one forward
    iN <- n `bitcast` ptr innerNode
    numElems' <- deref (metaOf ->> numElemsOf) n
    startIdx <- sub numElems' (int16 1)
    pos' <- load posPtr 0
    forLoop startIdx (`uge` pos') (`sub` int16 1) $ \i -> mdo
      j <- add i (int16 1)
      k <- add i (int16 2)
      assign (valueAt j) n =<< deref (valueAt i) n
      assign (childAt k) iN =<< deref (childAt j) iN
      increment int16 (childAt k ->> metaOf ->> posInParentOf) iN

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
  function "node_rebalance_or_split" args nodeSize $ \[n, root, idx] -> mdo
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
      splitterValue <- load splitter 0  -- TODO: deref?

      -- Move keys to left node
      leftNumElems <- deref (metaOf ->> numElemsOf) left
      assign (valueAt leftNumElems) left splitterValue

      leftSlotsOpen' <- sub leftSlotsOpen (int16 1)
      forLoop (int16 0) (`ult` leftSlotsOpen') (add (int16 1)) $ \i -> do
        j <- add leftNumElems (int16 1) >>= add i
        assign (valueAt j) left =<< deref (valueAt i) n

      store splitter 0 =<< deref (valueAt leftSlotsOpen') n

      -- Shift keys in this node to the left
      numElemsN <- deref (metaOf ->> numElemsOf) n
      idxEnd <- sub numElemsN leftSlotsOpen
      forLoop (int16 0) (`ult` idxEnd) (add (int16 1)) $ \i -> do
        -- TODO memmove possible?
        j <- add i leftSlotsOpen
        assign (valueAt i) n =<< deref (valueAt j) n

      -- And children (if necessary)
      isInnerNode <- deref (metaOf ->> nodeTypeOf) n >>= (`eq` innerNodeTypeVal)
      if' isInnerNode $ do
        iN <- n `bitcast` ptr innerNode
        iLeft <- left `bitcast` ptr innerNode

        -- Move children
        forLoop (int16 0) (`ult` leftSlotsOpen) (add (int16 1)) $ \i -> do
          leftNumElems <- deref (metaOf ->> numElemsOf) left
          leftPos <- add leftNumElems (int16 1) >>= add i
          -- TODO: check next part against C++ code
          assign (childAt leftPos) iLeft =<< deref (childAt i) iN
          assign (childAt leftPos ->> metaOf ->> parentOf) iLeft left
          assign (childAt leftPos ->> metaOf ->> posInParentOf) iLeft leftPos

        -- Shift child pointer to the left + update position
        endIdx <- sub numElemsN leftSlotsOpen >>= add (int16 1)
        forLoop (int16 0) (`ult` endIdx) (add (int16 1)) $ \i -> do
          j <- add i leftSlotsOpen
          assign (childAt i) iN =<< deref (childAt j) iN
          assign (childAt i ->> metaOf ->> posInParentOf) iN i

      -- Update node sizes
      update (metaOf ->> numElemsOf) left (`add` leftSlotsOpen)
      update (metaOf ->> numElemsOf) n (`sub` leftSlotsOpen)
      ret leftSlotsOpen

    split <- block `named` "split"
    -- Option B) split
    call splitFn $ (,[]) <$> [n, root]
    ret (int16 0)  -- No re-balancing
  where
    calculateLeftSlotsOpen numberOfKeys left idx = do
      -- TODO: check if casting needed?
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

  function "iterator_init" args void $ \[iter, cur, pos] -> do
    assign currentPtrOf iter cur
    assign valuePosOf iter pos

mkIteratorInitEnd :: Operand -> ModuleCodegen Operand
mkIteratorInitEnd iterInit = do
  iter <- typeOf Iterator
  node <- typeOf Node

  function "iterator_end_init" [(ptr iter, "iter")] void $ \[iter] -> do
    call iterInit $ (,[]) <$> [iter, nullPtr node, int16 0]
    retVoid

mkIteratorIsEqual :: ModuleCodegen Operand
mkIteratorIsEqual = do
  iter <- typeOf Iterator

  function "iterator_is_equal" [(ptr iter, "lhs"), (ptr iter, "rhs")] i1 $ \[lhs, rhs] -> mdo
    currentLhs <- deref currentPtrOf lhs
    currentRhs <- deref currentPtrOf rhs
    isEqualPtrs <- currentLhs `eq` currentRhs
    condBr isEqualPtrs equalPtrs notEqual

    equalPtrs <- block `named` "equal_pointers"
    valuePosLhs <- deref valuePosOf lhs
    valuePosRhs <- deref valuePosOf rhs
    isEqual <- valuePosLhs `eq` valuePosRhs
    condBr isEqual equal notEqual

    equal <- block `named` "equal"
    ret (bit 1)

    notEqual <- block `named` "notEqual"
    ret (bit 0)

mkIteratorCurrent :: ModuleCodegen Operand
mkIteratorCurrent = do
  iter <- typeOf Iterator
  value <- typeOf Value

  function "iterator_current" [(ptr iter, "iter")] (ptr value) $ \[iter] -> mdo
    valuePos <- deref valuePosOf iter
    valueAddr <- addr (currentOf ->> valueAt valuePos) iter
    ret valueAddr

mkIteratorNext :: ModuleCodegen Operand
mkIteratorNext = do
  iter <- typeOf Iterator
  node <- typeOf Node
  innerNode <- typeOf InnerNode

  function "iterator_next" [(ptr iter, "iter")] void $ \[iter] -> mdo
    isLeaf <- deref (currentOf ->> metaOf ->> nodeTypeOf) iter >>= (`eq` leafNodeTypeVal)
    condBr isLeaf leafNext innerNext

    leafNext <- block `named` "leaf_next"
    -- Case 1: Still elements left to iterate -> increment position
    increment int16 valuePosOf iter
    valuePos <- deref valuePosOf iter
    numElems <- deref (currentOf ->> metaOf ->> numElemsOf) iter
    hasNextInLeaf <- valuePos `ult` numElems
    condBr hasNextInLeaf end leafToNextInner

    leafToNextInner <- block `named` "leaf_next_inner"
    -- Case 2: at right-most element -> go to next inner node
    let loopCondition = do
          isNotNull <- deref currentPtrOf iter >>= (`ne` nullPtr node)
          pos' <- deref valuePosOf iter
          numElems' <- deref (currentOf ->> metaOf ->> numElemsOf) iter
          atEnd <- pos' `eq` numElems'
          isNotNull `and` atEnd
    whileLoop loopCondition $ do
      assign valuePosOf iter =<< deref (currentOf ->> metaOf ->> posInParentOf) iter
      assign currentPtrOf iter =<< deref (currentOf ->> metaOf ->> parentOf) iter

    innerNext <- block `named` "inner_next"
    -- Case 3: Go to left most child in inner node
    nextPos <- deref valuePosOf iter >>= add (int16 1)
    iCurrent <- deref currentPtrOf iter >>= (`bitcast` ptr innerNode)
    currentPtr <- allocate (ptr node) =<< deref (childAt nextPos) iCurrent
    let loopCondition' = do
          ty <- deref (metaOf ->> nodeTypeOf) =<< load currentPtr 0
          ty `eq` innerNodeTypeVal
    whileLoop loopCondition' $ do
      iCurrent <- load currentPtr 0 >>= (`bitcast` ptr innerNode)
      firstChild <- deref (childAt (int16 0)) iCurrent
      store currentPtr 0 firstChild

    assign currentPtrOf iter =<< load currentPtr 0
    assign valuePosOf iter (int16 0)
    retVoid

    end <- block `named` "end"
    retVoid

mkLinearSearchLowerBound :: Operand -> ModuleCodegen Operand
mkLinearSearchLowerBound compareValues = do
  value <- typeOf Value
  let args = [(ptr value, "val"), (ptr value, "current"), (ptr value, "end")]

  function "linear_search_lower_bound" args (ptr value) $ \[val, curr, end] -> mdo
    -- Finds an iterator to first element not less than given value.
    currentPtr <- allocate (ptr value) curr
    let loopCondition = do
          current <- load currentPtr 0
          current `ne` end
    whileLoop loopCondition $ mdo
      current <- load currentPtr 0
      result <- call compareValues [(current, []), (val, [])]
      isLessThan <- result `eq` int8 (-1)
      condBr isLessThan ltBlock gtOrEqBlock

      ltBlock <- block `named` "compare_lt"
      current' <- gep current [int32 1] -- TODO: check if correct
      store currentPtr 0 current'

    ret end

    gtOrEqBlock <- block `named` "compare_gt_or_eq"
    current <- load currentPtr 0
    ret current

mkLinearSearchUpperBound :: Operand -> ModuleCodegen Operand
mkLinearSearchUpperBound compareValues = do
  value <- typeOf Value
  let args = [(ptr value, "val"), (ptr value, "current"), (ptr value, "end")]

  function "linear_search_upper_bound" args (ptr value) $ \[val, curr, end] -> mdo
    -- Finds an iterator to first element that is greater than given value.
    currentPtr <- allocate (ptr value) curr
    let loopCondition = do
          current <- load currentPtr 0
          current `ne` end
    whileLoop loopCondition $ mdo
      current <- load currentPtr 0
      result <- call compareValues [(current, []), (val, [])]
      isGreaterThan <- result `eq` int8 1
      condBr isGreaterThan gtBlock ltOrEqBlock

      ltOrEqBlock <- block `named` "compare_lt_or_eq"
      current' <- gep current [int32 1] -- TODO: check if correct
      store currentPtr 0 current'

    ret end

    gtBlock <- block `named` "compare_gt"
    current <- load currentPtr 0
    ret current

mkBtreeInitEmpty :: ModuleCodegen ()
mkBtreeInitEmpty = do
  tree <- typeOf BTree
  node <- typeOf Node

  function "btree_init_empty" [(ptr tree, "tree")] void $ \[tree] -> mdo
    assign rootPtrOf tree (nullPtr node)
    assign firstPtrOf tree (nullPtr node)

  pure ()

mkBtreeInit :: Operand -> ModuleCodegen ()
mkBtreeInit btreeInsert = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  let args = [(ptr tree, "tree"), (ptr iter, "start"), (ptr iter, "end")]

  function "btree_init" args void $ \[tree, start, end] -> mdo
    call btreeInsert $ (,[]) <$> [tree, start, end]
    pure ()

  pure ()

mkBtreeCopy :: Operand -> Operand -> ModuleCodegen ()
mkBtreeCopy nodeClone isEmptyTree = do
  tree <- typeOf BTree
  node <- typeOf Node
  innerNode <- typeOf InnerNode

  function "btree_copy" [(ptr tree, "to"), (ptr tree, "from")] void $ \[to, from] -> mdo
    isSame <- to `eq` from
    condBr isSame earlyReturn differentBlock

    earlyReturn <- block `named` "early_return"
    retVoid

    differentBlock <- block `named` "different"
    isEmpty <- call isEmptyTree [(from, [])]
    condBr isEmpty earlyReturn copyBlock

    copyBlock <- block `named` "copy_tree"
    -- NOTE: this assumes from is still empty -> no memory cleanup
    root <- deref rootPtrOf from
    clonedRoot <- call nodeClone [(root, [])]
    assign rootPtrOf to clonedRoot

    tmpPtr <- allocate (ptr node) =<< deref rootPtrOf to
    let loopCondition = do
          tmp <- load tmpPtr 0
          ty <- deref (metaOf ->> nodeTypeOf) tmp
          ty `eq` innerNodeTypeVal
    whileLoop loopCondition $ do
      iTmp <- load tmpPtr 0 >>= (`bitcast` ptr innerNode)
      firstChild <- deref (childAt (int16 0)) iTmp
      store tmpPtr 0 firstChild

    tmp <- load tmpPtr 0
    assign firstPtrOf to tmp

  pure ()

mkBtreeDestroy :: Operand -> ModuleCodegen ()
mkBtreeDestroy btreeClear = do
  tree <- typeOf BTree

  function "btree_destroy" [(ptr tree, "tree")] void $ \[t] -> do
    call btreeClear [(t, [])]
    pure ()

  pure ()

mkBtreeIsEmpty :: ModuleCodegen Operand
mkBtreeIsEmpty = do
  tree <- typeOf BTree
  node <- typeOf Node

  function "btree_is_empty" [(ptr tree, "tree")] i1 $ \[t] -> do
    root <- deref rootPtrOf t
    isNull <- root `eq` nullPtr node
    ret isNull

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
    count <- call nodeCountEntries [(root, [])]
    ret count

mkBtreeInsertValue :: Operand -> Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeInsertValue nodeNew rebalanceOrSplit compareValues searchLowerBound searchUpperBound isEmptyTree = do
  tree <- typeOf BTree
  node <- typeOf Node
  value <- typeOf Value
  numberOfKeys <- numKeysAsOperand

  function "btree_insert_value" [(ptr tree, "tree"), (ptr value, "val")] i1 $ \[t, val] -> mdo
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
      insertInNonEmptyLeafNode loopBlock noInsert inserted t currentPtr current val numberOfKeys

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
      isEqual <- (bit 0 `eq`) =<< call compareValues ((,[]) <$> [valueAtPos, val])  -- Can we do a weak compare just by using pointers here?
      alreadyInserted <- notLast `and` isEqual
      condBr alreadyInserted noInsert continueInsert

      continueInsert <- block `named` "inner_continue_insert"
      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent
      br loopBlock  -- TODO: check if correctly generated

    insertInNonEmptyLeafNode loopBlock noInsert inserted t currentPtr current val numberOfKeys = mdo
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
      isEqual <- (bit 0 `eq`) =<< call compareValues ((,[]) <$> [valueAtPrevPos, val])  -- Can we do a weak compare just by using pointers here?
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
      forLoop numElems (`ugt` idx''') (`sub` int16 1) $ \j -> do
        -- TODO: memmove possible?
        j' <- sub j (int16 1)
        assign (valueAt j) current =<< deref (valueAt j') current

      value <- gep val [int32 0]
      assign (valueAt idx) current value
      update (metaOf ->> numElemsOf) current (add (int16 1))
      br inserted

mkBtreeInsertRange :: Operand -> Operand -> Operand -> Operand -> ModuleCodegen ()
mkBtreeInsertRange iterIsEqual iterCurrent iterNext btreeInsertValue = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  let args = [(ptr tree, "tree"), (ptr iter, "begin"), (ptr iter, "end")]

  function "btree_insert_range" args void $ \[t, begin, end] -> do
    let loopCondition = do
          isEqual <- call iterIsEqual $ (,[]) <$> [begin, end]
          not isEqual
    whileLoop loopCondition $ do
      val <- call iterCurrent [(begin, [])]
      call btreeInsertValue $ (,[]) <$> [t, val]
      call iterNext [(begin, [])]

  pure ()

mkBtreeBegin :: ModuleCodegen Operand
mkBtreeBegin = do
  tree <- typeOf BTree
  iter <- typeOf Iterator

  function "btree_begin" [(ptr tree, "tree"), (ptr iter, "result")] void $ \[t, result] -> do
    assign currentPtrOf result =<< deref firstPtrOf t
    assign valuePosOf result (int16 0)

mkBtreeEnd :: ModuleCodegen Operand
mkBtreeEnd = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  node <- typeOf Node

  function "btree_end" [(ptr tree, "tree"), (ptr iter, "result")] void $ \[t, result] -> do
    assign currentPtrOf result (nullPtr node)
    assign valuePosOf result (int16 0)

mkBtreeContains :: Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeContains iterIsEqual btreeFind btreeEnd = do
  tree <- typeOf BTree
  value <- typeOf Value

  function "btree_end" [(ptr tree, "tree"), (ptr value, "val")] i1 $ \[t, val] -> do
    iterPtr <- allocateIter
    endIterPtr <- allocateIter
    call btreeFind $ (,[]) <$> [t, val, iterPtr]
    call btreeEnd $ (,[]) <$> [t, endIterPtr]
    isEqual <- call iterIsEqual $ (,[]) <$> [iterPtr, endIterPtr]
    isNotEqual <- not isEqual
    ret isNotEqual

mkBtreeFind :: Operand -> Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeFind btreeEnd isEmptyTree searchLowerBound compareValues iterInit iterInitEnd = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  value <- typeOf Value
  let args = [(ptr tree, "tree"), (ptr value, "val"), (ptr iter, "result")]

  function "btree_find" args void $ \[t, val, result] -> mdo
    isEmpty <- call isEmptyTree [(t, [])]
    condBr isEmpty notFound doFind

    notFound <- block `named` "not_found"
    call btreeEnd $ (,[]) <$> [t, result]
    retVoid

    doFind <- block `named` "find"
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
      matchesVal <- call compareValues ((, []) <$> [pos, val]) >>= (`eq` bit 0)
      foundValue <- foundMatch `and` matchesVal
      condBr foundValue handleMatch continueFind

      handleMatch <- block `named` "handle_found_match"
      call iterInit $ (,[]) <$> [result, current, idx]
      br end

      continueFind <- block `named` "continue_find"
      isLeaf <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` leafNodeTypeVal)
      condBr isLeaf notFound searchChildren

      notFound <- block `named` "not_found"
      call iterInitEnd [(result, [])]
      br end

      searchChildren <- block `named` "search_in_children"
      -- Continue search in child node
      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent

    end <- block `named` "end"
    retVoid

mkBtreeLowerBound :: Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen ()
mkBtreeLowerBound isEmptyTree iterInit iterInitEnd searchLowerBound compareValues = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  value <- typeOf Value
  let args = [(ptr tree, "tree"), (ptr value, "val"), (ptr iter, "result")]

  function "btree_lower_bound" args void $ \[t, val, result] -> mdo
    isEmpty <- call isEmptyTree [(t, [])]
    condBr isEmpty lowerBoundEmpty lowerBoundNonEmpty

    lowerBoundEmpty <- block `named` "empty"
    call iterInitEnd [(result, [])]
    retVoid

    lowerBoundNonEmpty <- block `named` "non_empty"
    res <- allocateIter
    call iterInitEnd [(res, [])]
    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t

    loop $ mdo
      current <- load currentPtr 0
      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchLowerBound $ (,[]) <$> [val, first, last]
      idx <- pointerDiff i16 pos first
      isLeaf <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` leafNodeTypeVal)
      condBr isLeaf lowerBoundLeaf lowerBoundInner

      lowerBoundLeaf <- block `named` "lower_bound_leaf"
      isLast <- pos `eq` last
      condBr isLast handleLast handleOther

      handleLast <- block `named` "handle_last"
      copy currentPtrOf res result
      copy valuePosOf res result
      br end

      handleOther <- block `named` "handle_not_last"
      call iterInit $ (,[]) <$> [result, current, idx]
      br end

      lowerBoundInner <- block `named` "lower_bound_inner"
      isNotLast <- pos `ne` last
      -- Can the following be done with just pointer comparisons?
      matchesVal' <- call compareValues ((,[]) <$> [pos, val]) >>= (`eq` bit 0)
      matchFound <- isNotLast `and` matchesVal'
      condBr matchFound innerFound innerNotFound

      innerFound <- block `named` "inner_found"
      call iterInit $ (,[]) <$> [result, current, idx]
      br end

      innerNotFound <- block `named` "inner_not_found"
      if' isNotLast $ do
        call iterInit $ (,[]) <$> [res, current, idx]

      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent

    end <- block `named` "end"
    retVoid

  pure ()

mkBtreeUpperBound :: Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen ()
mkBtreeUpperBound isEmptyTree iterInit iterInitEnd searchUpperBound compareValues = do
  tree <- typeOf BTree
  iter <- typeOf Iterator
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  value <- typeOf Value
  let args = [(ptr tree, "tree"), (ptr value, "val"), (ptr iter, "result")]

  function "btree_upper_bound" args void $ \[t, val, result] -> mdo
    isEmpty <- call isEmptyTree [(t, [])]
    condBr isEmpty upperBoundEmpty upperBoundNonEmpty

    upperBoundEmpty <- block `named` "empty"
    call iterInitEnd [(result, [])]
    retVoid

    upperBoundNonEmpty <- block `named` "non_empty"
    res <- allocateIter
    call iterInitEnd [(res, [])]
    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t

    loop $ mdo
      current <- load currentPtr 0
      numElems <- deref (metaOf ->> numElemsOf) current
      first <- addr (valueAt (int16 0)) current
      last <- addr (valueAt numElems) current
      pos <- call searchUpperBound $ (,[]) <$> [val, first, last]
      idx <- pointerDiff i16 pos first
      isLeaf <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` leafNodeTypeVal)
      condBr isLeaf upperBoundLeaf upperBoundInner

      upperBoundLeaf <- block `named` "upper_bound_leaf"
      isLast <- pos `eq` last
      condBr isLast handleLast handleOther

      handleLast <- block `named` "handle_last"
      copy currentPtrOf res result
      copy valuePosOf res result
      br end

      handleOther <- block `named` "handle_not_last"
      call iterInit $ (,[]) <$> [result, current, idx]
      br end

      upperBoundInner <- block `named` "upper_bound_inner"
      -- Can the following be done with just pointer comparisons?
      isNotLast <- pos `ne` last
      if' isNotLast $ do
        call iterInit $ (,[]) <$> [result, current, idx]

      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent

    end <- block `named` "end"
    retVoid

  pure ()

mkBtreeClear :: Operand -> ModuleCodegen Operand
mkBtreeClear nodeDelete = do
  tree <- typeOf BTree
  node <- typeOf Node

  function "btree_clear" [(ptr tree, "tree")] void $ \[t] -> do
    root <- deref rootPtrOf t
    isNotNull <- root `ne` nullPtr node
    if' isNotNull $ do
      call nodeDelete [(root, [])]
      assign rootPtrOf t (nullPtr node)
      assign firstPtrOf t (nullPtr node)

mkBtreeSwap :: ModuleCodegen ()
mkBtreeSwap = do
  tree <- typeOf BTree

  function "btree_swap" [(ptr tree, "lhs"), (ptr tree, "rhs")] void $ \[lhs, rhs] ->
    for_ [rootPtrOf, firstPtrOf] $ \path ->
      swap path lhs rhs

  pure ()

mkBtreeAssign :: Operand -> Operand -> ModuleCodegen ()
mkBtreeAssign isEmptyTree nodeClone = do
  tree <- typeOf BTree
  node <- typeOf Node
  innerNode <- typeOf InnerNode

  function "btree_assign" [(ptr tree, "tree"), (ptr tree, "other")] void $ \[t, other] -> mdo
    isSame <- t `eq` other
    condBr isSame end emptyCheck

    emptyCheck <- block `named` "empty_check"
    isOtherEmpty <- call isEmptyTree [(other, [])]
    condBr isOtherEmpty end doAssign

    doAssign <- block `named` "assign_tree"
    -- Deep copy
    otherRoot <- deref rootPtrOf other
    assign rootPtrOf t =<< call nodeClone [(otherRoot, [])]

    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t
    let loopCondition = do
          ty <- deref (metaOf ->> nodeTypeOf) =<< load currentPtr 0
          ty `eq` innerNodeTypeVal
    whileLoop loopCondition $ mdo
      iCurrent <- load currentPtr 0 >>= (`bitcast` ptr innerNode)
      store currentPtr 0 =<< deref (childAt (int16 0)) iCurrent

    assign firstPtrOf t =<< load currentPtr 0
    br end

    end <- block `named` "end"
    retVoid

  pure ()

mkBtreeIsEqual :: Operand -> Operand -> Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen ()
mkBtreeIsEqual btreeBegin btreeEnd btreeContains btreeSize iterIsEqual iterNext iterCurrent = mdo
  tree <- typeOf BTree
  let args = [(ptr tree, "lhs"), (ptr tree, "rhs")]

  btreeIsEqual <- function "btree_is_equal" args i1 $ \[lhs, rhs] -> mdo
    isSame <- lhs `eq` rhs
    condBr isSame returnTrue notSame

    notSame <- block `named` "not_same_trees"
    sizeLhs <- call btreeSize [(lhs, [])]
    sizeRhs <- call btreeSize [(rhs, [])]
    sizesNotEqual <- sizeLhs `ne` sizeRhs
    condBr sizesNotEqual returnFalse continueCompare

    continueCompare <- block `named` "compare_trees"
    leftSmaller <- sizeLhs `ult` sizeRhs
    condBr leftSmaller flipComparison continueCompare'

    flipComparison <- block `named` "flip_comparison"
    result <- call btreeIsEqual $ (,[]) <$> [rhs, lhs]
    ret result

    continueCompare' <- block `named` "compare_trees"
    beginPtr <- allocateIter
    endPtr <- allocateIter
    call btreeBegin $ (,[]) <$> [rhs, beginPtr]
    call btreeEnd $ (,[]) <$> [rhs, endPtr]

    let loopCondition = do
          isEqual <- call iterIsEqual $ (,[]) <$> [beginPtr, endPtr]
          not isEqual
    whileLoop loopCondition $ mdo
      val <- call iterCurrent [(beginPtr, [])]
      leftContains <- call btreeContains $ (,[]) <$> [lhs, val]
      leftNotContains <- not leftContains
      condBr leftNotContains returnFalse advanceIter

      advanceIter <- block `named` "advance_iterator"
      call iterNext [(beginPtr, [])]

    br returnTrue

    returnTrue <- block `named` "trees_are_equal"
    ret (bit 1)

    returnFalse <- block `named` "trees_not_equal"
    ret (bit 0)

  pure ()

mkBtreeDepth :: Operand -> Operand -> ModuleCodegen ()
mkBtreeDepth isEmptyTree nodeDepth = do
  tree <- typeOf BTree

  function "btree_is_empty" [(ptr tree, "tree")] i64 $ \[t] -> mdo
    isEmpty <- call isEmptyTree [(t, [])]
    condBr isEmpty emptyBlk notEmptyBlk

    emptyBlk <- block `named` "empty"
    ret (int64 0)

    notEmptyBlk <- block `named` "not_empty"
    root <- deref currentPtrOf t
    result <- call nodeDepth [(root, [])]
    ret result

  pure ()

mkBtreeCountNodes :: Operand -> Operand -> ModuleCodegen ()
mkBtreeCountNodes isEmptyTree nodeCountEntries = do
  tree <- typeOf BTree

  function "btree_is_empty" [(ptr tree, "tree")] i64 $ \[t] -> mdo
    isEmpty <- call isEmptyTree [(t, [])]
    condBr isEmpty emptyBlk notEmptyBlk

    emptyBlk <- block `named` "empty"
    ret (int64 0)

    notEmptyBlk <- block `named` "not_empty"
    root <- deref currentPtrOf t
    result <- call nodeCountEntries [(root, [])]
    ret result

  pure ()


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

numKeysAsOperand :: ModuleCodegen Operand
numKeysAsOperand = do
  metadata <- asks meta
  pure $ int16 $ toInteger $ numKeys metadata

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
  | IteratorIdx
  | TreeIdx
  | ArrayOf Index
  | PtrOf Index

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

currentPtrOf :: Path 'IteratorIdx ('PtrOf 'NodeIdx)
currentPtrOf = mkPath [int32 0]

currentOf :: Path 'IteratorIdx 'NodeIdx
currentOf = mkPath [int32 0, int32 0]

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
        Iterator -> panic "sizeOf: not implemented for iterator"
        BTree -> panic "sizeOf: not implemented for btree"
      X64 -> case dt of
        NodeType -> 1
        Node -> 256
        InnerNode -> 424
        Value -> columnCount * 4
        NodeSize -> 2
        Iterator -> panic "sizeOf: not implemented for iterator"
        BTree -> panic "sizeOf: not implemented for btree"

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
  call memsetFn [ (p', [])
                , (int8 0, [])
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

-- TODO: check if state is not updated inside helper function, always ask for latest data of struct
-- TODO: early returns in for and if are broken?
-- TODO: check all branches at end of loops, rewrite if needed.. make it impossible to make a mistake?
-- TODO: check how "isSet" is configured for normal btree..
