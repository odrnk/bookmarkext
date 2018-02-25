{-# LANGUAGE DeriveGeneric #-}
module DBI.Sort
    ( Order(..)
    , Sort(..)
    , sortsToIdentifiers
    , sortsToOrderBy
    ) where

import Database.PostgreSQL.Simple.Types
import Data.Text as Txt (pack)
import Data.Aeson.Types
import GHC.Generics
import Data.Char (toLower, toUpper)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (unpack)

data Order = Asc | Desc deriving (Show, Generic)
data Sort = Sort { text :: String, ord :: Order } deriving (Show, Generic)

sortsToIdentifiers :: [Sort] -> [Identifier]
sortsToIdentifiers = map $ Identifier . Txt.pack . text

sortsToOrderBy :: [Sort] -> String
sortsToOrderBy [] = ""
sortsToOrderBy sorts = " order by" ++ body
    where body = tail $ sorts >>= (\s -> ", ? " ++ show (ord s))

customOptions = genericParseJSON defaultOptions
    { constructorTagModifier = map toLower
    }

instance FromJSON Order where
    parseJSON = customOptions

instance FromJSON Sort where
    parseJSON = withObject "sort" $ \v -> case HashMap.keys v of
        [key] -> Sort (unpack key)
                    <$> (parseJSON $ head $ HashMap.elems v)
        _ -> typeMismatch "sort" emptyObject
