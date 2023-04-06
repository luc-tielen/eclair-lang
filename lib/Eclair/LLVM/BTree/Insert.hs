{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.LLVM.BTree.Insert
  ( mkBtreeInsertValue
  , mkBtreeInsertRangeTemplate
  ) where

import Prelude hiding (void)
import Eclair.LLVM.Table
import Eclair.LLVM.BTree.Types

mkNodeSplitPoint :: ModuleCodegen Operand
mkNodeSplitPoint = mdo
  nodeSize <- typeOf NodeSize
  numberOfKeys <- numKeysAsOperand

  function "eclair_btree_node_split_point" [] nodeSize $ \_ -> mdo
    a' <- mul (int16 3) numberOfKeys
    a <- udiv a' (int16 4)
    b <- sub numberOfKeys (int16 2)
    ret =<< minimum' Unsigned a b

mkSplit :: Operand -> Operand -> Operand -> ModuleCodegen Operand
mkSplit nodeNew nodeSplitPoint growParent = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  numberOfKeys <- numKeysAsOperand

  function "eclair_btree_node_split" [(ptr node, "node"), (ptr (ptr node), "root")] void $ \[n, root] -> mdo
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
      loopFor splitPoint' (`ule` numberOfKeys) (add (int16 1)) $ \i -> mdo
        j <- load jPtr 0
        iChild <- deref (childAt i) iN
        assign (metaOf ->> parentOf) iChild sibling
        assign (metaOf ->> posInParentOf) iChild j
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

  function "eclair_btree_node_grow_parent" [(ptr node, "node"), (ptr (ptr node), "root"), (ptr node, "sibling")] void $
    \[n, root, sibling] -> mdo
    parent <- deref (metaOf ->> parentOf) n
    isNull <- parent `eq` nullPtr node
    numElems <- deref (metaOf ->> numElemsOf) n
    condBr isNull createNewRoot insertNewNodeInParent

    createNewRoot <- blockNamed "create_new_root"
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

    insertNewNodeInParent <- blockNamed "insert_new_node_in_parent"
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

  insertInner <- function "eclair_btree_node_insert_inner" args void $
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
  function "eclair_btree_node_rebalance_or_split" args nodeSize $ \[n, root, idx] -> mdo
    -- TODO assert(n->meta.num_elements == NUM_KEYS);

    parent <- deref (metaOf ->> parentOf) n >>= (`bitcast` ptr innerNode)
    pos <- deref (metaOf ->> posInParentOf) n
    hasParent <- parent `ne` nullPtr node
    posGTZero <- pos `ugt` int16 0
    shouldRebalance <- and hasParent posGTZero
    condBr shouldRebalance rebalance split

    rebalance <- blockNamed "rebalance"
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
    split <- blockNamed "split"
    -- Option B) split
    _ <- call splitFn [n, root]
    ret (int16 0)  -- No re-balancing
  where
    calculateLeftSlotsOpen numberOfKeys left' idx = do
      numElems <- deref (metaOf ->> numElemsOf) left'
      openSlots <- sub numberOfKeys numElems
      isLessThan <- openSlots `slt` idx
      select isLessThan openSlots idx

mkBtreeInsertValue :: Operand -> Operand -> Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeInsertValue nodeNew compareValues searchLowerBound searchUpperBound isEmptyTree = mdo
  tree <- typeOf BTree
  node <- typeOf Node
  value <- typeOf Value
  numberOfKeys <- numKeysAsOperand

  splitPoint <- mkNodeSplitPoint
  split <- mkSplit nodeNew splitPoint growParent
  growParent <- mkGrowParent nodeNew insertInner
  insertInner <- mkInsertInner rebalanceOrSplit
  rebalanceOrSplit <- mkRebalanceOrSplit split

  function "eclair_btree_insert_value" [(ptr tree, "tree"), (ptr value, "val")] i1 $ \[t, val] -> mdo
    isEmpty <- call isEmptyTree [t]
    condBr isEmpty emptyCase nonEmptyCase

    emptyCase <- blockNamed "empty"
    leaf <- call nodeNew [leafNodeTypeVal]
    assign (metaOf ->> numElemsOf) leaf (int16 1)
    assign (valueAt (int16 0)) leaf =<< load val 0

    assign rootPtrOf t leaf
    assign firstPtrOf t leaf
    br inserted

    nonEmptyCase <- blockNamed "non_empty"
    -- Insert using iterative approach
    currentPtr <- allocate (ptr node) =<< deref rootPtrOf t
    loop $ mdo
      loopBlock <- currentBlock
      current <- load currentPtr 0
      isInner <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` innerNodeTypeVal)
      condBr isInner inner leaf

      inner <- blockNamed "inner"
      insertInNonEmptyInnerNode loopBlock noInsert currentPtr current val

      leaf <- blockNamed "leaf"
      insertInNonEmptyLeafNode rebalanceOrSplit noInsert inserted t currentPtr current val numberOfKeys

    noInsert <- blockNamed "no_insert"
    ret (bit 0)

    inserted <- blockNamed "inserted_new_value"
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

      continueInsert <- blockNamed "inner_continue_insert"
      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent
      br loopBlock

    insertInNonEmptyLeafNode rebalanceOrSplit noInsert inserted t currentPtr current val numberOfKeys = mdo
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

      continueInsert <- blockNamed "leaf_continue_insert"
      nodeIsFull <- numElems `uge` numberOfKeys
      condBr nodeIsFull split noSplit

      split <- blockNamed "split"
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

      noSplit <- blockNamed "no_split"
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
    function "eclair_btree_insert_range" args void $ \[t, begin, end] -> do
      let loopCondition = do
            isEqual <- call (ipIterIsEqual iterParams) [begin, end]
            not' isEqual
      loopWhile loopCondition $ do
        -- NOTE: Can directly insert value in other btree, same array type!
        val <- call (ipIterCurrent iterParams) [begin]
        _ <- call btreeInsertValue [t, val]
        call (ipIterNext iterParams) [begin]
