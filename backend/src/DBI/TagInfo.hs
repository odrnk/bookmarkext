module DBI.TagInfo
( TagInfo(..)
) where

import Database.PostgreSQL.Simple.FromRow
import Data.UUID (UUID)

data TagInfo = TagInfo
    { id :: UUID
    , name :: String
    } deriving (Show)

instance FromRow TagInfo where
    fromRow = TagInfo <$> field <*> field
