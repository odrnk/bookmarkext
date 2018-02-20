{-# LANGUAGE OverloadedStrings #-}
module Main where

import DBI
import WebApp
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    putStrLn $ "http://localhost:18080/"
    run 18080 app
