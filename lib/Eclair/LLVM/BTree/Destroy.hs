{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eclair.LLVM.BTree.Destroy
  ( mkNodeDelete
  , mkBtreeDestroy
  , mkBtreeClear
  ) where

import Prelude hiding (void)
import Eclair.LLVM.BTree.Types

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

mkBtreeDestroy :: Operand -> ModuleCodegen Operand
mkBtreeDestroy btreeClear = do
  tree <- typeOf BTree

  function "btree_destroy" [(ptr tree, "tree")] void $ \[t] -> do
    _ <- call btreeClear [t]
    pass

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
