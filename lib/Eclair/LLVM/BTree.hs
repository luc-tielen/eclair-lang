{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Eclair.LLVM.BTree
  ( Meta(..)
  , SearchIndex
  , SearchType(..)
  , codegen
  ) where

import Prelude hiding (void, swap)
import Control.Monad.Morph
import Eclair.LLVM.Codegen
import Eclair.LLVM.Table
import Eclair.LLVM.BTree.Types
import Eclair.LLVM.BTree.Create
import Eclair.LLVM.BTree.Destroy
import Eclair.LLVM.BTree.Compare
import Eclair.LLVM.BTree.Iterator
import Eclair.LLVM.BTree.Insert
import Eclair.LLVM.BTree.Find
import Eclair.LLVM.BTree.Bounds
import Eclair.LLVM.BTree.Size


codegen :: Externals -> ConfigT (TemplateT Meta IO) Table
codegen exts = do
  sizes <- computeSizes
  lift $ hoist intoIO $ do
    tys <- generateTypes sizes
    runReaderT generateTableFunctions $ CGState tys sizes exts
  where intoIO = pure . runIdentity

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
  nodeCountEntries <- mkNodeCountEntries
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
  btreeInsert <- mkBtreeInsertValue nodeNew compareValues searchLowerBound searchUpperBound isEmptyTree
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
  btreeClear <- mkBtreeClear
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
