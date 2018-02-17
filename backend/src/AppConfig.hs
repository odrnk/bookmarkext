{-# LANGUAGE OverloadedStrings #-}
module AppConfig
    ( getConfigInfo
    , ConfigInfo(..)
    ) where

import Data.Configurator
import Data.Configurator.Types
import Data.Word

data ConfigInfo = ConfigInfo
    { dbpassword :: String
    , dbport :: Word16
    , dbname :: String
    } deriving(Show)

getConfigInfo :: IO ConfigInfo
getConfigInfo = do
    cfg <- load [Required "backend.cfg"]
    dbpassword <- lookupDefault "" cfg "dbpassword"
    dbport <- lookupDefault 5432 cfg "dbport"
    dbname <- lookupDefault "bm" cfg "dbname"
    return $ ConfigInfo dbpassword dbport dbname
