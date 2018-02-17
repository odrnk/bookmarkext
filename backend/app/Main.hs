{-# LANGUAGE OverloadedStrings #-}
module Main where

import DBI
import Data.UUID (UUID, fromString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField

main :: IO ()
main = do
    print "asdf"
