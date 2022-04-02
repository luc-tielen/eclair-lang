{-# LANGUAGE TemplateHaskell #-}

module Eclair.EIR.IR
  ( EIR(..)
  , EIRF(..)
  , Relation
  , Type(..)
  , Function(..)
  , LabelId(..)
  ) where

import Protolude hiding (Type,Meta)
import Data.String (IsString(..))
import Eclair.Syntax ( Id, Number )
import Eclair.RA.IndexSelection (Index)
import Eclair.Runtime.Metadata
import Data.Functor.Foldable.TH

type Relation = Id
type LowerBound = EIR
type UpperBound = EIR

data Type
  = Program
  | Value
  | Iter
  | Pointer Type
  deriving (Eq, Show)

data Function
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
  | Function Text [Type] EIR
  | FunctionArg Int
  | DeclareProgram [Metadata]
  | FieldAccess EIR Int
  | Var Text
  | Assign EIR EIR
  | Call Function [EIR]
  | HeapAllocateProgram
  | FreeProgram EIR
  | StackAllocate Type Relation -- NOTE: always need to know for which relation
  | Par [EIR]
  | Loop [EIR]
  | If EIR EIR
  | Not EIR
  | And EIR EIR
  | Equals EIR EIR
  | Jump LabelId
  | Label LabelId
  | Return EIR
  | Lit Number
  deriving (Eq, Show)

makeBaseFunctor ''EIR
