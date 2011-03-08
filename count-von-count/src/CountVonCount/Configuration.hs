-- | Global configuration
--
module CountVonCount.Configuration
    ( Configuration (..)
    , loadConfiguration
    , loadConfigurationFromFile
    ) where

import Control.Applicative ((<$>))

import Data.Object (lookupObject, fromMapping)
import Data.Object.Yaml (YamlObject, toYamlScalar, decodeFile)

import CountVonCount.StationMap

data Configuration = Configuration
    { configurationStationMap :: StationMap
    } deriving (Show)

loadConfiguration :: YamlObject -> Maybe Configuration
loadConfiguration object = do
    m <- fromMapping object
    Configuration <$> load loadStationMap "Station map" m
  where
    load f k m = f =<< lookupObject (toYamlScalar k) m

loadConfigurationFromFile :: FilePath -> IO (Maybe Configuration)
loadConfigurationFromFile path = do
    yaml <- decodeFile path 
    return $ loadConfiguration =<< yaml
