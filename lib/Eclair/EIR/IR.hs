{-# LANGUAGE TemplateHaskell #-}

module Eclair.EIR.IR
  ( EIR(..)
  , EIRF(..)
  , Relation
  , EIRType(..)
  , EIRFunction(..)
  , LabelId(..)
  ) where

import Protolude hiding (Meta)
import Data.String (IsString(..))
import Eclair.Syntax ( Id, Number )
import Eclair.RA.IndexSelection (Index)
import Eclair.Runtime.Metadata
import Data.Functor.Foldable.TH

type Relation = Id
type LowerBound = EIR
type UpperBound = EIR

-- TODO: remove prefix?
data EIRType
  = Program
  | Value
  | Iter
  | Pointer EIRType
  deriving (Eq, Show)

-- TODO: remove prefix
data EIRFunction
  = InitializeEmpty
  | Destroy
  | Purge
  | Swap
  | Merge
  | IsEmpty
  | Contains
  | Insert
  | IterCurrent
  | IterNext
  | IterIsEqual
  | IterLowerBound
  | IterUpperBound
  deriving (Eq, Show)

newtype LabelId
  = LabelId Text
  deriving (Eq, Show)

instance IsString LabelId where
  fromString = LabelId . fromString

data EIR
  = Block [EIR]
  | Function Text [EIRType] EIR
  | FunctionArg Int
  | DeclareType [Metadata] -- TODO: DeclareProgram?
  | FieldAccess EIR Int
  | Var Text
  | Assign EIR EIR
  | Call EIRFunction [EIR]
  | HeapAllocate EIRType  -- TODO: HeapAllocateProgram?
  | FreeProgram EIR
  | StackAllocate EIRType Relation -- NOTE: always need to know for which relation
  | Par [EIR]
  | Loop [EIR]
  | If EIR EIR
  | Not EIR
  | And EIR EIR
  | Equals EIR EIR
  | Jump LabelId
  | Label LabelId
  | Return EIR
  | RangeQuery Relation Index LowerBound UpperBound EIR  -- TODO: remove?
  | Lit Number
  deriving (Eq, Show)

makeBaseFunctor ''EIR
