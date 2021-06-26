module Eclair.RA.IR
  ( Relation
  , RA(..)
  , RAClause
  , Action
  , ColumnIndex
  ) where

import Eclair.Syntax ( Id, Number )
import Protolude


type Relation = Id
type RAClause = RA
type Action = RA
type ColumnIndex = Int

-- NOTE: removed Insert, couldn't find a use?
data RA
  = Search Relation [RAClause] Action
  | Project Relation [RA]
  | Merge Relation Relation
  | Swap Relation Relation
  | Purge Relation
  | Seq RA RA
  -- | Par [RA]
  | Loop RA
  | Exit RAClause

  | RAModule [RA]
  | RALit Number
  | ColumnIndex Relation ColumnIndex
  | RAConstraint RA RA  -- TODO simplify names
  deriving (Eq, Show)
