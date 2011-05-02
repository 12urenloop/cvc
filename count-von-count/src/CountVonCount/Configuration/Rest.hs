-- | Rest service configuration
--
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleContexts #-}
module CountVonCount.Configuration.Rest
    ( RestConfiguration (..)
    , loadRestConfiguration
    ) where

import Control.Applicative (Applicative, (<$>), (<*>))

import Control.Failure (Failure)
import Data.Object (fromMapping, ObjectExtractError)
import Data.Object.Yaml (YamlObject)
import Data.ByteString (ByteString)

import CountVonCount.Configuration.Util

data RestConfiguration = RestConfiguration
    { restHost :: ByteString
    , restPort :: Int
    , restPath :: ByteString
    } deriving (Show)

loadRestConfiguration :: (Applicative m, Failure ObjectExtractError m)
                      => YamlObject -> m RestConfiguration
loadRestConfiguration object = do
    mapping <- fromMapping object
    RestConfiguration <$> lookupString "Host" mapping
                      <*> fmap read (lookupString "Port" mapping)
                      <*> lookupString "Path" mapping
