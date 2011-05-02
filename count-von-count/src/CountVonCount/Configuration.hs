-- | Global configuration
--
{-# LANGUAGE FlexibleContexts #-}
module CountVonCount.Configuration
    ( Configuration (..)
    , stationPosition
    , allowedMac
    , prettifyMac
    , loadConfiguration
    , loadConfigurationFromFile
    ) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M

import Data.ByteString (ByteString)
import Control.Failure (Failure)
import Data.Object (lookupObject, fromMapping, ObjectExtractError)
import Data.Object.Yaml (YamlObject, toYamlScalar, decodeFile, IsYamlScalar)

import CountVonCount.Types
import CountVonCount.Configuration.Rest
import CountVonCount.Configuration.Criteria
import CountVonCount.Configuration.Util

data Configuration = Configuration
    { configurationRest       :: RestConfiguration
    , configurationStations   :: Map Station Position
    , configurationCsvLog     :: FilePath
    , configurationCriteria   :: [Criterium]
    , configurationMacs       :: Map Mac ByteString
    , configurationListenPort :: Int
    , configurationAdminPort  :: Int
    , configurationVerbosity  :: Verbosity
    }

stationPosition :: Station -> Configuration -> Maybe Position
stationPosition station = M.lookup station . configurationStations

allowedMac :: Mac -> Configuration -> Bool
allowedMac mac = M.member mac . configurationMacs

prettifyMac :: Mac -> Configuration -> ByteString
prettifyMac mac = fromMaybe mac . M.lookup mac . configurationMacs

loadConfiguration :: (Applicative m, Failure ObjectExtractError m)
                  => YamlObject -> m Configuration
loadConfiguration object = do
    m <- fromMapping object
    Configuration <$> load loadRestConfiguration "Rest API" m
                  <*> fmap (M.map read) (load loadMap "Stations" m)
                  <*> lookupString "CSV log" m
                  <*> load loadCriteria "Criteria" m
                  <*> load loadMap "Macs" m
                  <*> fmap read (lookupString "Listen port" m)
                  <*> fmap read (lookupString "Admin port" m)
                  <*> fmap read (lookupString "Verbosity" m)
  where
    load f k m = f =<< lookupObject (toYamlScalar k) m

loadConfigurationFromFile :: FilePath -> IO Configuration
loadConfigurationFromFile path = do
    yaml <- decodeFile path 
    loadConfiguration =<< yaml
