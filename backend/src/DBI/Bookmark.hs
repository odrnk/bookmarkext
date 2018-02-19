module DBI.Bookmark
    ( Bookmark(..)
    ) where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import Data.Time.LocalTime (LocalTime)
import Data.Aeson.Types (Value)
import Data.UUID (UUID)
import Data.Int
import Data.ByteString

data Bookmark = Bookmark {
    id :: UUID
  , url :: String
  , title :: String
  , description :: String
  , visitCount :: Int16
  , lastVisitdate :: LocalTime
  , snapshotUrl :: String
  , dateAdded :: LocalTime
  , dateModified :: LocalTime
  , faviconData :: Maybe (Binary ByteString)
  , tags :: Value
} deriving (Show)

instance FromRow Bookmark where
    fromRow = Bookmark <$> field <*> field <*> field <*> field
        <*> field <*> field <*> field <*> field <*> field
        <*> field <*> field
