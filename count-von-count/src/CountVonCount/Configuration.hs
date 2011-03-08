-- | Global configuration
--
module CountVonCount.Configuration
    ( Configuration (..)
    , loadConfiguration
    , loadConfigurationFromFile
    ) where

import Control.Applicative ((<$>), (<*>))

import Data.Object (lookupObject, lookupScalar, fromMapping)
import Data.Object.Yaml (YamlObject, toYamlScalar, decodeFile, fromYamlScalar)

import CountVonCount.Types
import CountVonCount.Configuration.StationMap
import CountVonCount.Configuration.Criteria

data Configuration = Configuration
    { configurationStationMap :: StationMap
    , configurationCsvLog     :: FilePath
    , configurationCriteria   :: [Criterium]
    }

loadConfiguration :: YamlObject -> Maybe Configuration
loadConfiguration object = do
    m <- fromMapping object
    Configuration <$> load loadStationMap "Station map" m
                  <*> lookup' "CSV log" m
                  <*> load loadCriteria "Criteria" m
  where
    load f k m = f =<< lookupObject (toYamlScalar k) m
    lookup' k m = fromYamlScalar <$> lookupScalar (toYamlScalar k) m

loadConfigurationFromFile :: FilePath -> IO (Maybe Configuration)
loadConfigurationFromFile path = do
    yaml <- decodeFile path 
    return $ loadConfiguration =<< yaml
