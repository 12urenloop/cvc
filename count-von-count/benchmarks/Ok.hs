-- | A web server that happily consumes every request
--
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import System.Environment (getArgs)

import Snap.Types (modifyResponse, setResponseCode)
import Snap.Http.Server (httpServe, emptyConfig, addListen, ConfigListen (..))

main :: IO ()
main = do
    [port] <- map read <$> getArgs
    let config = addListen (ListenHttp "0.0.0.0" port) emptyConfig
    httpServe config $ modifyResponse $ setResponseCode 204
