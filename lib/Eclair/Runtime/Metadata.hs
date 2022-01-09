module Eclair.Runtime.Metadata
  ( Metadata(..)
  , mkMeta
  ) where

import Protolude hiding (Type)
import Eclair.RA.IndexSelection (Index(..))
import Eclair.TypeSystem
import qualified Eclair.Runtime.BTree as BTree

newtype Metadata
  = BTree BTree.Meta
  deriving (Eq, Show)
  -- TODO: support other datastructures (Trie, ...)

mkMeta :: Index -> [Type] -> Metadata
mkMeta (Index columns) ts =
  -- TODO: choose datastructure based on index/types
  BTree $ BTree.Meta
    { BTree.numColumns = length ts
    , BTree.index = columns
    , BTree.blockSize = 256
    , BTree.searchType = BTree.Linear
    }
