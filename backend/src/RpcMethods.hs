{-# LANGUAGE OverloadedStrings #-}
module RpcMethods
    ( handleRpcRequest
    ) where

import Network.JsonRpc.Server
import Control.Monad.Trans (liftIO)
-- import Control.Monad.Reader (ReaderT, ask, runReaderT)
-- import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Data.ByteString.Lazy
import Data.Maybe
import qualified DBI as DBI

-- type Server = ReaderT (MVar Integer) IO
methods :: [Method IO]
methods = [add, getRootTags]

handleRpcRequest :: ByteString -> IO ByteString
handleRpcRequest request = do
    response <- call methods request :: IO (Maybe ByteString)
    return $ fromMaybe "" response

add :: Method IO
add = toMethod "Add" f (Required "id" :+: Required "y" :+: ())
    where f :: String -> Double -> RpcResult IO String
          f x y = liftIO $ return $ x ++ (show y)

getRootTags :: Method IO
getRootTags = toMethod "GetRootTags" f params
    where
        f :: [DBI.Sort] -> Int -> Int -> RpcResult IO [DBI.BaseTag]
        f sorts skip take = liftIO $ grt sorts skip take
        params = Required "sort" :+: Required "skip" :+: Required "take" :+: ()

grt :: [DBI.Sort] -> Int -> Int -> IO [DBI.BaseTag]
grt sorts skip take = do
    conn <- DBI.getDbConnection
    DBI.getRootTags conn sorts skip take
