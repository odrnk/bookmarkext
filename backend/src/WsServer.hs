{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module WsServer
    ( wsApp
    ) where

import qualified Network.WebSockets as WS
import Data.Text (Text, append)

wsApp :: WS.ServerApp
wsApp pendingConn = do
    conn <- WS.acceptRequest pendingConn
    WS.sendTextData conn ("Hello, client!" :: Text)
    msg <- WS.receiveData conn
    WS.sendTextData conn (append ("You sent: " :: Text) msg )

class Foo a b c | a b -> c where
    bar :: a -> b -> c

instance Foo Bool Bool Int where
    bar x y = 123

instance Foo Bool Bool Char where
    bar x y = 'c'
