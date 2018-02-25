{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module DBI.TagInfo
    ( TagInfo(..)
    ) where

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types
import Data.UUID (UUID)
import Data.Aeson.Types
import GHC.Generics

data TagInfo = TagInfo
    { id :: UUID
    , name :: String
    } deriving (Show, Generic)

instance FromRow TagInfo where
    fromRow = TagInfo <$> field <*> field

instance FromJSON TagInfo

instance FromField [TagInfo] where
    fromField f mdata = do
        value <- (fromField f mdata :: Conversion Value)
        case (fromJSON value) of
            Success xs -> return xs
            Error msg -> error msg -- this can happen only if there is a bug
