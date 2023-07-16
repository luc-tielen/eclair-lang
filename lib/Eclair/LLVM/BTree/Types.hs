module Eclair.LLVM.BTree.Types
  ( Meta(..)
  , Types(..)
  , SearchIndex
  , SearchType(..)
  , Sizes(..)
  , CGState(..)
  , IRCodegen
  , ModuleCodegen
  , Index(..)
  , metaOf
  , valuesOf
  , valueAt
  , parentOf
  , posInParentOf
  , numElemsOf
  , nodeTypeOf
  , baseOf
  , childrenOf
  , childAt
  , currentPtrOf
  , valuePosOf
  , rootPtrOf
  , firstPtrOf
  , DataType(..)
  , typeOf
  , allocateIter
  , memset
  , leafNodeTypeVal
  , innerNodeTypeVal
  , numKeysAsOperand
  , numKeysHelper
  , numKeys
  , module Eclair.LLVM.Externals
  , module Eclair.LLVM.Codegen
  ) where

import Eclair.LLVM.Codegen
import Eclair.LLVM.Hash
import Eclair.LLVM.Externals
import Prettyprinter

data Meta
  = Meta
  { numColumns :: Int        -- Amount of columns each node has
  , index :: SearchIndex     -- Which columns are used to index values
  , blockSize :: Word64      -- Number of bytes per btree node
  , searchType :: SearchType -- Search strategy used in a single node
  }
  deriving stock (Eq, Ord, Show)
  deriving stock Generic
  deriving ToHash via HashWithPrefix "btree" Meta

instance Pretty Meta where
  pretty meta =
    "num_columns=" <> pretty (numColumns meta) <> comma <+>
    -- TODO: use "withCommas"
    "index=" <> brackets (Prelude.fold $ intersperse comma $ map pretty (index meta)) <> comma <+>
    "block_size=" <> pretty (blockSize meta) <> comma <+>
    "search_type=" <> pretty (searchType meta)

type Column = Int

type SearchIndex = [Column]

data SearchType = Linear | Binary
  deriving stock (Eq, Ord, Show)
  deriving stock (Generic, Enum)

instance ToHash SearchType where
  getHash = \case
    Linear -> getHash ("linear" :: Text)
    Binary -> getHash ("binary" :: Text)

instance Pretty SearchType where
  pretty Linear = "linear"
  pretty Binary = "binary"

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

data Sizes
  = Sizes
  { pointerSize :: Word64
  , valueSize :: Word64
  , nodeDataSize :: Word64
  , leafNodeSize :: Word64
  , innerNodeSize :: Word64
  }

-- State used during rest of the btree codegen
data CGState
  = CGState
  { types :: Types
  , typeSizes :: Sizes
  , externals :: Externals
  }

type IRCodegen = IRBuilderT ModuleCodegen

type ModuleCodegen = ReaderT CGState (Template Meta)

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

valuesOf :: Path 'NodeIdx ('ArrayOf 'ValueIdx)
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

childrenOf :: Path 'InnerNodeIdx ('ArrayOf 'NodeIdx)
childrenOf = mkPath [int32 1]

childAt :: Operand -> Path 'InnerNodeIdx ('PtrOf 'NodeIdx)
childAt idx = mkPath [int32 1, idx]

currentPtrOf :: Path 'IteratorIdx ('PtrOf 'NodeIdx)
currentPtrOf = mkPath [int32 0]

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
  let p' = ptrcast i8 p
  _ <- call memsetFn [ p'
                     , int8 $ fromIntegral val
                     , int64 (fromIntegral byteCount)
                     , bit 0
                     ]
  pass

leafNodeTypeVal, innerNodeTypeVal :: Operand
leafNodeTypeVal = bit 0
innerNodeTypeVal = bit 1

numKeys :: Meta -> Sizes -> Word64
numKeys settings sizes =
  numKeysHelper settings nodeMetaSize valueByteSize
  where
    nodeMetaSize = nodeDataSize sizes
    valueByteSize = valueSize sizes

-- NOTE: Where possible, use the more userfriendly numKeys function
numKeysHelper :: Meta -> Word64 -> Word64 -> Word64
numKeysHelper settings nodeMetaSize valueByteSize =
  max 3 desiredNumberOfKeys
  where
    blockByteSize = blockSize settings
    valuesByteSize =
      if blockByteSize > nodeMetaSize
      then blockByteSize - nodeMetaSize
      else 0
    desiredNumberOfKeys = valuesByteSize `div` valueByteSize

numKeysAsOperand :: ModuleCodegen Operand
numKeysAsOperand = do
  metadata <- getParams
  sizes <- asks typeSizes
  pure $ int16 $ toInteger $ numKeys metadata sizes

-- NOTE: this only allocates on stack, but doesn't initialize it,
-- this still needs to happen in rest of the code
allocateIter :: IRCodegen Operand
allocateIter = do
  iter <- typeOf Iterator
  alloca iter (Just (int32 1)) 0
