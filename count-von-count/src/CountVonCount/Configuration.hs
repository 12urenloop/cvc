-- | Global configuration
--
{-# LANGUAGE FlexibleContexts #-}
module CountVonCount.Configuration
    ( Configuration (..)
    , stationPosition
    , allowedMac
    , aboveRssiTreshold
    , noticeMeasurement
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
    { configurationRest         :: RestConfiguration
    , configurationStations     :: Map Station Position
    , configurationCsvLog       :: FilePath
    , configurationCriteria     :: [Criterium]
    , configurationMacs         :: Map Mac ByteString
    , configurationListenPort   :: Int
    , configurationRssiTreshold :: Int
    , configurationAdminPort    :: Int
    , configurationVerbosity    :: Verbosity
    }

stationPosition :: Station -> Configuration -> Maybe Position
stationPosition station = M.lookup station . configurationStations

allowedMac :: Mac -> Configuration -> Bool
allowedMac mac = M.member mac . configurationMacs

prettifyMac :: Mac -> Configuration -> ByteString
prettifyMac mac = fromMaybe mac . M.lookup mac . configurationMacs

aboveRssiTreshold :: Rssi -> Configuration -> Bool
aboveRssiTreshold rssi = (<= rssi) . configurationRssiTreshold

-- | Should we take this measurement into account?
--
noticeMeasurement :: Mac -> Measurement -> Configuration -> Bool
noticeMeasurement mac (_, _, rssi) conf =
    allowedMac mac conf && aboveRssiTreshold rssi conf

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
                  <*> fmap read (lookupString "RSSI treshold" m)
                  <*> fmap read (lookupString "Admin port" m)
                  <*> fmap read (lookupString "Verbosity" m)
  where
    load f k m = f =<< lookupObject (toYamlScalar k) m

loadConfigurationFromFile :: FilePath -> IO Configuration
loadConfigurationFromFile path = do
    yaml <- decodeFile path 
    loadConfiguration =<< yaml
