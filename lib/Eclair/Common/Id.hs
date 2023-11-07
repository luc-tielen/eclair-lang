module Eclair.Common.Id
  ( Id(..)
  , prependToId
  , appendToId
  , startsWithId
  , stripIdPrefixes
  , startsWithIdPrefix
  , startsWithDeltaPrefix
  , deltaPrefix
  , newPrefix
  ) where

import Eclair.Datalog
import qualified Data.Text as T
import Prettyprinter


newtype Id = Id { unId :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Marshal

instance Pretty Id where
  pretty = pretty . unId


appendToId :: Id -> Text -> Id
appendToId (Id x) y = Id (x <> y)

prependToId :: Text -> Id -> Id
prependToId x (Id y) = Id (x <> y)

startsWithId :: Id -> Id -> Bool
startsWithId (Id x) (Id start) =
  start `T.isPrefixOf` x

stripIdPrefixes :: Id -> Id
stripIdPrefixes (Id x) = Id $ stripPrefixes x where
  stripPrefixes t = foldl' stripPrefix t [deltaPrefix, newPrefix]
  stripPrefix acc prefix = fromMaybe acc (T.stripPrefix prefix acc)

-- TODO: make all prefixes starts with special symbol, invalid in syntax
deltaPrefix, newPrefix :: Text
deltaPrefix = "delta_"
newPrefix = "new_"

startsWithIdPrefix :: Id -> Bool
startsWithIdPrefix (Id x) =
  any (`T.isPrefixOf` x) [deltaPrefix, newPrefix]

startsWithDeltaPrefix :: Id -> Bool
startsWithDeltaPrefix (Id x) =
  deltaPrefix `T.isPrefixOf` x
