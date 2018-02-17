module DBI.Bookmark
    ( Bookmark(..)
    ) where

-- import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types
import Data.Time.LocalTime (LocalTime)
import Data.Aeson.Types (Value)
import Data.UUID (UUID, fromString)
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

    -- id uuid, url text, title text, description text, visit_count smallint,
    -- last_visit_date timestamp, snapshot_url text, date_added timestamp,
    -- date_modified timestamp, favicon_data bytea, tags jsonb

