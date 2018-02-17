{-# LANGUAGE OverloadedStrings #-}
-- Database Interface
module DBI
    ( getDbConnection
    , createBookmark
    , Tag
    , BaseTag
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types
import Data.Time.LocalTime (LocalTime)
import Data.Time.Calendar (Day)
import Data.Aeson.Types (Value)
import Data.UUID (UUID, fromString)
import Data.Int
import AppConfig
import Data.Text as T (pack)
import Data.ByteString.Char8 as C8 (pack)
import Data.Maybe
import DBI.Bookmark as B

getDbConnectionInfo :: IO ConnectInfo
getDbConnectionInfo = do
    cfg <- getConfigInfo
    return defaultConnectInfo {
        connectDatabase = (dbname cfg)
      , connectPort = (dbport cfg)
      , connectPassword = (dbpassword cfg)
    }

getDbConnection :: IO Connection
getDbConnection = do
    connectionInfo <- getDbConnectionInfo
    connect connectionInfo

createBookmark :: Connection -> String -> String -> String -> Int16 -> LocalTime -> [String] -> IO UUID
createBookmark conn url title description visitCount lastVisitDate tags = do
    let queryStr = "select bm.create_bookmark(?::text, ?::text, ?::text, ?::smallint, ?::timestamp, ?::text[])"
    let input = (url, title, description, visitCount, lastVisitDate, PGArray tags)
    [Only id] <- query conn queryStr input :: IO [Only UUID]
    return id

-- getUtcNow :: IO LocalTime
-- getUtcNow = do
--     utcNow <- getCurrentTime
--     return $ utcToLocalTime utc utcNow

-- -- test createBookmark
-- createBookmark123 = do
--     conn <- getDbConnection
--     now <- getUtcNow
--     id <- createBookmark conn "test_url23" "asdf23 title" "as2 desc" 11 now []
--     return id

data Tag = Tag {
      base :: BaseTag
    , tags :: Value
} deriving (Show)

data BaseTag = BaseTag {
      id :: UUID
    , name :: String
    , dateAdded :: LocalTime
    , dateModified :: LocalTime
} deriving (Show)

instance FromRow BaseTag where
    fromRow = BaseTag <$> field <*> field <*> field <*> field

instance FromRow Tag where
    fromRow = Tag <$> fromRow <*> field

data Order = Asc | Desc deriving (Show)
data Sort = Sort { text :: String, ord :: Order } deriving (Show)


sortsToIdentifiers :: [Sort] -> [Identifier]
sortsToIdentifiers = map $ Identifier . T.pack . text

sortsToOrderBy :: [Sort] -> String
sortsToOrderBy [] = ""
sortsToOrderBy sorts = " order by" ++ body
    where body = tail $ sorts >>= (\s -> ", ? " ++ show (ord s))

getRootTags :: Connection -> [Sort] -> Int -> Int -> IO [BaseTag]
getRootTags conn sorts skip take = query conn queryStr $ (sortsToIdentifiers sorts) :. (skip, take)
    where queryStr = Query $ C8.pack $ "select * from bm.root_tag" ++ (sortsToOrderBy sorts) ++ " offset ? limit ?"

getChildTags :: Connection -> [Sort] -> Int -> Int -> UUID -> IO [Tag]
getChildTags conn sorts skip take tagId = query conn queryStr $ [tagId] :. (sortsToIdentifiers sorts) :. (skip, take)
    where queryStr = Query $ C8.pack $ "select * from bm.get_child_tags(?)" ++ (sortsToOrderBy sorts) ++ " offset ? limit ?"

getBookmarksOfTag :: Connection -> [Sort] -> Int -> Int -> UUID -> IO [B.Bookmark]
getBookmarksOfTag conn sorts skip take tagId = query conn queryStr $ [tagId] :. (sortsToIdentifiers sorts) :. (skip, take)
    where queryStr = Query $ C8.pack $ "select * from bm.get_bookmarks_of_tag(?)" ++ (sortsToOrderBy sorts) ++ " offset ? limit ?"
