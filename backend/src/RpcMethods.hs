{-# LANGUAGE OverloadedStrings #-}
module RpcMethods
    ( add
    , aaa
    ) where

import Network.JsonRpc.Server
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)

type Server = ReaderT (MVar Integer) IO

add = toMethod "Add" f (Required "x" :+: Required "y" :+: ())
    where f :: Double -> Double -> RpcResult Server Double
          f x y = liftIO $ return (x + y)

aaa :: Int
aaa request = do
    response <- runReaderT (call [add] request) count
    B.putStrLn $ fromMaybe "" response
