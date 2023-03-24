{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.LLVM.BTree.Size
  ( mkNodeCountEntries
  , mkBtreeIsEmpty
  , mkBtreeSize
  ) where

import Prelude hiding (void)
import Eclair.LLVM.BTree.Types

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

    nullBlock <- blockNamed "null"
    ret (int64 0)

    notNullBlock <- blockNamed "not_null"
    count <- call nodeCountEntries [root]
    ret count
