{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Config
    ( Config (..)
    , defaultConfig
    , readConfigFile
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Data.Maybe (fromMaybe)

import Data.Aeson (FromJSON (..), ToJSON (..), (.=), (.:?), (.!=))
import Data.Yaml (decodeFile)
import qualified Data.Aeson as A

import CountVonCount.Types

data Config = Config
    { configCircuitLength :: Double
    , configSensorPort    :: Int
    , configLog           :: FilePath
    , configReplayLog     :: FilePath
    , configStations      :: [Station]
    , configBatons        :: [Baton]
    , configRssiThreshold :: Double
    } deriving (Show)

instance ToJSON Config where
    toJSON conf = A.object
        [ "circuitLength" .= configCircuitLength conf
        , "sensorPort"    .= configSensorPort    conf
        , "log"           .= configLog           conf
        , "replayLog"     .= configReplayLog     conf
        , "stations"      .= configStations      conf
        , "batons"        .= configBatons        conf
        , "rssiThreshold" .= configRssiThreshold conf
        ]

instance FromJSON Config where
    parseJSON (A.Object o) = Config <$>
        o .:? "circuitLength" .!= configCircuitLength defaultConfig <*>
        o .:? "sensorPort"    .!= configSensorPort    defaultConfig <*>
        o .:? "log"           .!= configLog           defaultConfig <*>
        o .:? "replayLog"     .!= configReplayLog     defaultConfig <*>
        o .:? "stations"      .!= configStations      defaultConfig <*>
        o .:? "batons"        .!= configBatons        defaultConfig <*>
        o .:? "rssiThreshold" .!= configRssiThreshold defaultConfig

    parseJSON _ = mzero

defaultConfig :: Config
defaultConfig = Config
    { configCircuitLength = 400
    , configSensorPort    = 9001
    , configLog           = "log/count-von-count.log"
    , configReplayLog     = "log/replay.log"
    , configStations      = []
    , configBatons        = []
    , configRssiThreshold = 20.0 -- TODO: better value
    }

readConfigFile :: FilePath -> IO Config
readConfigFile filePath = fromMaybe
    (error $ "Could not read config: " ++ filePath) <$> decodeFile filePath
