-- | Global configuration
--
module CountVonCount.Configuration
    ( Configuration (..)
    , loadConfiguration
    , loadConfigurationFromFile
    ) where

import Control.Applicative ((<$>), (<*>))

import Data.Object (lookupObject, fromMapping)
import Data.Object.Yaml (YamlObject, toYamlScalar, decodeFile)

import CountVonCount.Types
import CountVonCount.Configuration.Rest
import CountVonCount.Configuration.StationMap
import CountVonCount.Configuration.Criteria
import CountVonCount.Configuration.Util

data Configuration = Configuration
    { configurationRest       :: RestConfiguration
    , configurationStationMap :: StationMap
    , configurationCsvLog     :: FilePath
    , configurationCriteria   :: [Criterium]
    }

loadConfiguration :: YamlObject -> Maybe Configuration
loadConfiguration object = do
    m <- fromMapping object
    Configuration <$> load loadRestConfiguration "Rest API" m
                  <*> load loadStationMap "Station map" m
                  <*> lookupString "CSV log" m
                  <*> load loadCriteria "Criteria" m
  where
    load f k m = f =<< lookupObject (toYamlScalar k) m

loadConfigurationFromFile :: FilePath -> IO (Maybe Configuration)
loadConfigurationFromFile path = do
    yaml <- decodeFile path 
    return $ loadConfiguration =<< yaml
