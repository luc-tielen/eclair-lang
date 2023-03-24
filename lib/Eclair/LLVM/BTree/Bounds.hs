{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.LLVM.BTree.Bounds
  ( mkLinearSearchLowerBound
  , mkLinearSearchUpperBound
  , mkBtreeLowerBound
  , mkBtreeUpperBound
  ) where

import Prelude hiding (void)
import Eclair.LLVM.BTree.Types

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

        handleLast <- blockNamed "handle_last"
        copy currentPtrOf res result
        copy valuePosOf res result
        retVoid

        handleOther <- blockNamed "handle_not_last"
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

        handleLast <- blockNamed "handle_last"
        copy currentPtrOf res result
        copy valuePosOf res result
        retVoid

        handleOther <- blockNamed "handle_not_last"
        _ <- call iterInit [result, current, idx]
        retVoid

      -- Can the following be done with just pointer comparisons?
      isNotLast <- pos `ne` last
      if' isNotLast $ do
        call iterInit [result, current, idx]

      iCurrent <- current `bitcast` ptr innerNode
      store currentPtr 0 =<< deref (childAt idx) iCurrent
