{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.LLVM.BTree.Create
  ( mkNodeNew
  , mkBtreeInit
  , mkBtreeInitEmpty
  , mkBtreeSwap
  ) where

import Prelude hiding (void, swap)
import Eclair.LLVM.BTree.Types


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

mkBtreeSwap :: ModuleCodegen Operand
mkBtreeSwap = do
  tree <- typeOf BTree

  function "btree_swap" [(ptr tree, "lhs"), (ptr tree, "rhs")] void $ \[lhs, rhs] ->
    for_ [rootPtrOf, firstPtrOf] $ \path ->
      swap path lhs rhs
