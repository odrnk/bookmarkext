{-# LANGUAGE DeriveGeneric #-}
module DBI.Tag
    ( Tag(..)
    , BaseTag(..)
    ) where

import Database.PostgreSQL.Simple.FromRow
import Data.UUID (UUID)
import DBI.TagInfo as TI
import Data.Time.LocalTime (LocalTime)
import Data.Aeson.Types
import GHC.Generics

data Tag = Tag {
      base :: BaseTag
    , tags :: [TI.TagInfo]
} deriving (Show, Generic)

data BaseTag = BaseTag {
      id :: UUID
    , name :: String
    , dateAdded :: LocalTime
    , dateModified :: LocalTime
} deriving (Show, Generic)

instance FromRow BaseTag where
    fromRow = BaseTag <$> field <*> field <*> field <*> field

instance FromRow Tag where
    fromRow = Tag <$> fromRow <*> field


customOptions = defaultOptions
                { fieldLabelModifier = camelTo2 '_'
                }

instance ToJSON BaseTag where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions
