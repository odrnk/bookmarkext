{-# LANGUAGE OverloadedStrings #-}
-- Database Interface
module DBI
    ( getDbConnection
    , createBookmark
    , getRootTags
    , Sort (..)
    , Order (..)
    , Tag (..)
    , BaseTag (..)
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Types
import Data.Time.LocalTime (LocalTime, utcToLocalTime, utc)
import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime)
import Data.Aeson.Types (Value)
import Data.UUID (UUID, fromString)
import Data.Int
import AppConfig
import Data.ByteString.Char8 as C8 (pack)
import Data.Maybe
import DBI.Bookmark as B
import DBI.TagInfo as TI
import DBI.Tag
import DBI.Sort

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

-- for manual testing. TODO: move it somewhere
getUtcNow :: IO LocalTime
getUtcNow = do
    utcNow <- getCurrentTime
    return $ utcToLocalTime utc utcNow

-- -- test createBookmark
-- createBookmark123 = do
--     conn <- getDbConnection
--     now <- getUtcNow
--     id <- createBookmark conn "test_url23" "asdf23 title" "as2 desc" 11 now []
--     return id

getRootTags :: Connection -> [Sort] -> Int -> Int -> IO [BaseTag]
getRootTags conn sorts skip take = query conn queryStr $ (sortsToIdentifiers sorts) :. (skip, take)
    where queryStr = Query $ C8.pack $ "select * from bm.root_tag" ++ (sortsToOrderBy sorts) ++ " offset ? limit ?"

getChildTags :: Connection -> [Sort] -> Int -> Int -> UUID -> IO [Tag]
getChildTags conn sorts skip take tagId = query conn queryStr $ [tagId] :. (sortsToIdentifiers sorts) :. (skip, take)
    where queryStr = Query $ C8.pack $ "select * from bm.get_child_tags(?)" ++ (sortsToOrderBy sorts) ++ " offset ? limit ?"

getBookmarksOfTag :: Connection -> [Sort] -> Int -> Int -> UUID -> IO [B.Bookmark]
getBookmarksOfTag conn sorts skip take tagId = query conn queryStr $ [tagId] :. (sortsToIdentifiers sorts) :. (skip, take)
    where queryStr = Query $ C8.pack $ "select * from bm.get_bookmarks_of_tag(?)" ++ (sortsToOrderBy sorts) ++ " offset ? limit ?"

getRootBookmarks :: Connection -> [Sort] -> Int -> Int -> IO [B.Bookmark]
getRootBookmarks conn sorts skip take = query conn queryStr $ (sortsToIdentifiers sorts) :. (skip, take)
    where queryStr = Query $ C8.pack $ "select * from bm.root_bookmark" ++ (sortsToOrderBy sorts) ++ " offset ? limit ?"

getBookmarks :: Connection -> [Sort] -> Int -> Int -> IO [B.Bookmark]
getBookmarks conn sorts skip take = query conn queryStr $ (sortsToIdentifiers sorts) :. (skip, take)
    where queryStr = Query $ C8.pack $ "select * from bm.get_bookmarks()" ++ (sortsToOrderBy sorts) ++ " offset ? limit ?"

getAscendantTags :: Connection -> [Sort] -> Int -> Int -> UUID -> IO [TI.TagInfo]
getAscendantTags conn sorts skip take tagId = query conn queryStr $ [tagId] :. (sortsToIdentifiers sorts) :. (skip, take)
    where queryStr = Query $ C8.pack $ "select * from bm.get_ascendant_tags(?)" ++ (sortsToOrderBy sorts) ++ " offset ? limit ?"

setParentTag :: Connection -> UUID -> UUID -> IO Bool
setParentTag conn tagId parentTagId = do
    let queryStr = "select bm.set_parent_tag(?, ?)"
    [Only isSet] <- query conn queryStr [tagId, parentTagId] :: IO [Only Bool]
    return isSet

unsetParentTag :: Connection -> UUID -> UUID -> IO Int64
unsetParentTag conn tagId parentTagId = execute conn queryStr [tagId, parentTagId]
    where queryStr = "DO $$ BEGIN perform bm.unset_parent_tag(?, ?); END $$;"

resetParentTag :: Connection -> UUID -> UUID -> UUID -> IO Bool
resetParentTag conn tagId oldParentTagId newParentTagId = do
    let queryStr = "select bm.reset_parent_tag(?, ?)"
    [Only isSet] <- query conn queryStr [tagId, oldParentTagId, newParentTagId] :: IO [Only Bool]
    return isSet

createRootTag :: Connection -> String -> IO UUID
createRootTag conn name = do
    let queryStr = "select bm.create_root_tag(?::text)"
    [Only id] <- query conn queryStr [name] :: IO [Only UUID]
    return id

createTag :: Connection -> String -> UUID -> IO UUID
createTag conn name parentTagId = do
    let queryStr = "select bm.create_tag(?::text, ?::uuid)"
    [Only id] <- query conn queryStr (name, parentTagId) :: IO [Only UUID]
    return id

deleteBookmark :: Connection -> UUID -> IO Int64
deleteBookmark conn bookmarkId = execute conn queryStr [bookmarkId]
    where queryStr = "DO $$ BEGIN perform bm.delete_bookmark(?); END $$;"

deleteTag :: Connection -> UUID -> IO Int64
deleteTag conn tagId = execute conn queryStr [tagId]
    where queryStr = "DO $$ BEGIN perform bm.delete_tag(?); END $$;"

editBookmark :: Connection -> UUID -> String -> String -> String -> [String] -> IO Int64
editBookmark conn bookmarkId url title description tags = do
    let queryStr = "DO $$ BEGIN \
        \ perform bm.edit_bookmark(?::uuid, ?::text, ?::text, ?::text, ?::text[]); \
        \ END $$;"
    let input = (bookmarkId, url, title, description, PGArray tags)
    execute conn queryStr input

updateVisitInfo :: Connection -> String -> LocalTime -> Int16 -> IO Int64
updateVisitInfo conn url lastVisitDate visitCount = execute conn queryStr (url, lastVisitDate, visitCount)
    where queryStr = "DO $$ BEGIN \
        \ perform bm.update_visit_info(?::text, ?::timestamp, ?::smallint); \
        \ END $$;"
