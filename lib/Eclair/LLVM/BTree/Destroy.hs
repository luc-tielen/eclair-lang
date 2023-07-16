{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.LLVM.BTree.Destroy
  ( mkBtreeDestroy
  , mkBtreeClear
  ) where

import Prelude hiding (void)
import Eclair.LLVM.BTree.Types


mkBtreeDestroy :: Operand -> ModuleCodegen Operand
mkBtreeDestroy btreeClear = do
  tree <- typeOf BTree

  function "eclair_btree_destroy" [(ptr tree, "tree")] void $ \[t] -> do
    _ <- call btreeClear [t]
    pass

mkNodeDelete :: ModuleCodegen Operand
mkNodeDelete = mdo
  node <- typeOf Node
  innerNode <- typeOf InnerNode
  free <- asks (extFree . externals)

  nodeDelete <- function "eclair_btree_node_delete" [(ptr node, "node")] void $ \[n] -> mdo
    nodeTy <- deref (metaOf ->> nodeTypeOf) n
    isInner <- nodeTy `eq` innerNodeTypeVal
    if' isInner $ do  -- Delete children of inner node
      let inner = ptrcast innerNode n

      numElements <- deref (metaOf ->> numElemsOf) n
      loopFor (int16 0) (`ule` numElements) (add (int16 1)) $ \i -> mdo
        child <- deref (childAt i) inner
        isNotNull <- child `ne` nullPtr node
        if' isNotNull $
          call nodeDelete [child]

    let memory = ptrcast i8 n
    _ <- call free [memory]
    pass

  pure nodeDelete

mkBtreeClear :: ModuleCodegen Operand
mkBtreeClear = do
  tree <- typeOf BTree
  node <- typeOf Node

  nodeDelete <- mkNodeDelete

  function "eclair_btree_clear" [(ptr tree, "tree")] void $ \[t] -> do
    root <- deref rootPtrOf t
    isNotNull <- root `ne` nullPtr node
    if' isNotNull $ do
      _ <- call nodeDelete [root]
      assign rootPtrOf t (nullPtr node)
      assign firstPtrOf t (nullPtr node)
