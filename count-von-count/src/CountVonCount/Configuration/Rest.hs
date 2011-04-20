-- | Rest service configuration
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module CountVonCount.Configuration.Rest
    ( RestConfiguration (..)
    , loadRestConfiguration
    ) where

import Control.Applicative ((<$>), (<*>))

import Data.Object (fromMapping)
import Data.Object.Yaml (YamlObject)
import Data.ByteString (ByteString)

import CountVonCount.Configuration.Util

data RestConfiguration = RestConfiguration
    { restHost :: ByteString
    , restPort :: Int
    , restPath :: ByteString
    } deriving (Show)

loadRestConfiguration :: YamlObject -> Maybe RestConfiguration
loadRestConfiguration object = do
    mapping <- fromMapping object
    RestConfiguration <$> lookupString "Host" mapping
                      <*> fmap read (lookupString "Port" mapping)
                      <*> lookupString "Path" mapping
