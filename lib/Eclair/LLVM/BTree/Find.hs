{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.LLVM.BTree.Find
  ( mkBtreeContains
  , mkBtreeFind
  ) where

import Prelude hiding (void)
import Eclair.LLVM.BTree.Types


mkBtreeContains :: Operand -> Operand -> Operand -> ModuleCodegen Operand
mkBtreeContains iterIsEqual btreeFind btreeEnd = do
  tree <- typeOf BTree
  value <- typeOf Value

  function "eclair_btree_contains" [(ptr tree, "tree"), (ptr value, "val")] i1 $ \[t, val] -> do
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

  function "eclair_btree_find" args void $ \[t, val, result] -> mdo
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
      let iCurrent = ptrcast innerNode current
      store currentPtr 0 =<< deref (childAt idx) iCurrent
