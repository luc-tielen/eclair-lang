module Eclair.RA.IR
  ( Relation
  , RA(..)
  , Alias
  , Clause
  , Action
  , ColumnIndex
  ) where

import Eclair.Syntax ( Id, Number )
import Protolude


type Relation = Id
type Alias = Id
type Clause = RA
type Action = RA
type ColumnIndex = Int

-- NOTE: removed Insert, couldn't find a use?
data RA
  = Search Relation Alias [Clause] Action
  | Project Relation [RA]
  | Merge Relation Relation
  | Swap Relation Relation
  | Purge Relation
  | Seq [RA]
  -- | Par [RA]
  | Loop RA
  | Exit [Relation] -- NOTE: counttuples check is 'builtin' atm
                    -- Later this needs to be changed to Clause to deal with 'X<100' etc as well.
  | Module [RA]
  | Lit Number
  | ColumnIndex Relation ColumnIndex
  | Constrain RA RA
  deriving (Eq, Show)
