{-# LANGUAGE OverloadedStrings #-}
module WsServer
    ( wsApp
    ) where

import qualified Network.WebSockets as WS
import Data.Text (Text, append)
import RpcMethods

wsApp :: WS.ServerApp
wsApp pendingConn = do
    conn <- WS.acceptRequest pendingConn
    WS.sendTextData conn ("Hello, client!" :: Text)
    msg <- WS.receiveData conn
    resp <- handleRpcRequest msg
    WS.sendTextData conn resp
