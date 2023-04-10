{-# LANGUAGE DataKinds, UndecidableInstances #-}
module Facts where

import Data.Word
import qualified Language.Eclair as E
import GHC.Generics

newtype Vertex = Vertex Word32
  deriving (Generic, E.Marshal)
  deriving E.Fact
  via E.FactOptions Vertex 'E.Input "vertex"

data Edge = Edge Word32 Word32
  deriving (Generic, E.Marshal)
  deriving E.Fact
  via E.FactOptions Edge 'E.Input "edge"

newtype Reachable = Reachable Word32
  deriving (Generic, E.Marshal)
  deriving E.Fact
  via E.FactOptions Edge 'E.Output "reachable"

data Handle = Handle
  deriving E.Program
  via E.ProgramOptions Handle '[Vertex, Edge, Reachable]
