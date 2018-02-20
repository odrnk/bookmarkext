{-# LANGUAGE OverloadedStrings #-}
module WebApp
    ( app
    ) where

import WsServer
import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.HTTP.Types (status400)
import qualified Network.WebSockets as WS

app :: Application
app = websocketsOr WS.defaultConnectionOptions wsApp backupApp
    where
        backupApp :: Application
        backupApp _ respond = respond $
            responseLBS status400 [] "Not a WebSocket request"
