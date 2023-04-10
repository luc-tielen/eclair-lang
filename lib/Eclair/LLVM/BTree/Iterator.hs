{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.LLVM.BTree.Iterator
  ( mkIteratorInit
  , mkIteratorInitEnd
  , mkIteratorIsEqual
  , mkIteratorCurrent
  , mkIteratorNext
  , mkBtreeBegin
  , mkBtreeEnd
  ) where

import Prelude hiding (void)
import Eclair.LLVM.BTree.Types

mkIteratorInit :: ModuleCodegen Operand
mkIteratorInit = do
  iter <- typeOf Iterator
  node <- typeOf Node
  nodeSize <- typeOf NodeSize
  let args = [(ptr iter, "iter"), (ptr node, "cur"), (nodeSize, "pos")]

  function "eclair_btree_iterator_init" args void $ \[it, cur, pos] -> do
    assign currentPtrOf it cur
    assign valuePosOf it pos

mkIteratorInitEnd :: Operand -> ModuleCodegen Operand
mkIteratorInitEnd iterInit = do
  iter <- typeOf Iterator
  node <- typeOf Node

  function "eclair_btree_iterator_end_init" [(ptr iter, "iter")] void $ \[it] -> do
    _ <- call iterInit [it, nullPtr node, int16 0]
    retVoid

mkIteratorIsEqual :: ModuleCodegen Operand
mkIteratorIsEqual = do
  iter <- typeOf Iterator

  function "eclair_btree_iterator_is_equal" [(ptr iter, "lhs"), (ptr iter, "rhs")] i1 $ \[lhs, rhs] -> mdo
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

  function "eclair_btree_iterator_current" [(ptr iter, "iter")] (ptr value) $ \[it] -> mdo
    valuePos <- deref valuePosOf it
    currentNode <- deref currentPtrOf it
    ret =<< addr (valueAt valuePos) currentNode

mkIteratorNext :: ModuleCodegen Operand
mkIteratorNext = do
  iter <- typeOf Iterator

  function "eclair_btree_iterator_next" [(ptr iter, "iter")] void $ \[it] -> mdo
    current <- deref currentPtrOf it
    isInner <- deref (metaOf ->> nodeTypeOf) current >>= (`eq` innerNodeTypeVal)
    if' isInner $ do
      innerIterNext leafNextBlock it
      retVoid

    leafNextBlock <- leafIterNext it
    pass
  where
    leafIterNext iter = mdo
      leafNextBlock <- blockNamed "leaf.next"
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
            nullBlock <- blockNamed "leaf.no_parent"
            br endLoopCondition

            notNullBlock <- blockNamed "leaf.has_parent"
            pos' <- deref valuePosOf iter
            current' <- deref currentPtrOf iter
            numElems' <- deref (metaOf ->> numElemsOf) current'
            atEnd <- pos' `eq` numElems'

            br endLoopCondition

            endLoopCondition <- blockNamed "loop.condition.end"
            phi [(bit 0, nullBlock), (atEnd, notNullBlock)]
      loopWhile loopCondition $ do
        current' <- deref currentPtrOf iter
        assign valuePosOf iter =<< deref (metaOf ->> posInParentOf) current'
        assign currentPtrOf iter =<< deref (metaOf ->> parentOf) current'

      pure leafNextBlock

    innerIterNext leafNext iter = mdo
      node <- typeOf Node
      innerNode <- typeOf InnerNode
      -- Case 3: Go to left most child in inner node (a leaf node)
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

      currentLeaf <- load currentPtr 0
      assign currentPtrOf iter currentLeaf
      assign valuePosOf iter (int16 0)

      -- Leaf nodes may be empty due to biased insertion => go to next
      isNotEmpty <- deref (metaOf ->> numElemsOf) currentLeaf >>= (`ne` int16 0)
      if' isNotEmpty $ do
        retVoid

      br leafNext

mkBtreeBegin :: ModuleCodegen Operand
mkBtreeBegin = do
  tree <- typeOf BTree
  iter <- typeOf Iterator

  function "eclair_btree_begin" [(ptr tree, "tree"), (ptr iter, "result")] void $ \[t, result] -> do
    assign currentPtrOf result =<< deref firstPtrOf t
    assign valuePosOf result (int16 0)

mkBtreeEnd :: Operand -> ModuleCodegen Operand
mkBtreeEnd iteratorInitEnd = do
  tree <- typeOf BTree
  iter <- typeOf Iterator

  function "eclair_btree_end" [(ptr tree, "tree"), (ptr iter, "result")] void $ \[_t, result] -> do
    _ <- call iteratorInitEnd [result]
    pass
