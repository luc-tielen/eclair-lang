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
import Eclair.AST.IR (Number)
import Eclair.Id
import Eclair.RA.IndexSelection (Index)
import Eclair.LLVM.Metadata
import Data.Functor.Foldable.TH

type Relation = Id
type LowerBound = EIR
type UpperBound = EIR

data Type
  = Program
  | Value
  | Iter
  | Pointer Type
  | Void
  deriving (Eq, Show)

data Function
  = InitializeEmpty
  | Destroy
  | Purge
  | Swap
  | InsertRange
  | IsEmpty
  | Contains
  | Insert
  | IterCurrent
  | IterNext
  | IterIsEqual
  | IterLowerBound
  | IterUpperBound
  | IterBegin
  | IterEnd
  deriving (Eq, Show)

newtype LabelId
  = LabelId Text
  deriving (Eq, Show)

instance IsString LabelId where
  fromString = LabelId . fromString

data EIR
  = Block [EIR]
  | Function Text [Type] Type EIR
  | FunctionArg Int
  | DeclareProgram [(Relation, Metadata)]
  | FieldAccess EIR Int
  | Var Text
  | Assign EIR EIR
  | Call Relation Index Function [EIR]
  | HeapAllocateProgram
  | FreeProgram EIR
  | StackAllocate Relation Index Type
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
