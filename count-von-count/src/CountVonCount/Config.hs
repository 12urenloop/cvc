{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Config
    ( BoxxyConfig (..)
    , Config (..)
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
import CountVonCount.Boxxy

data Config = Config
    { configCircuitLength         :: Double
    , configMaxSpeed              :: Double
    , configBatonWatchdogLifespan :: Int
    , configBatonWatchdogInterval :: Int
    , configSensorPort            :: Int
    , configLog                   :: FilePath
    , configReplayLog             :: FilePath
    , configStations              :: [Station]
    , configBatons                :: [Baton]
    , configRssiThreshold         :: Double
    , configBoxxies               :: [BoxxyConfig]
    , configWebPort               :: Int
    } deriving (Show)

instance ToJSON Config where
    toJSON conf = A.object
        [ "circuitLength"         .= configCircuitLength         conf
        , "maxSpeed"              .= configMaxSpeed              conf
        , "batonWatchdogLifespan" .= configBatonWatchdogLifespan conf
        , "batonWatchdogInterval" .= configBatonWatchdogInterval conf
        , "sensorPort"            .= configSensorPort            conf
        , "log"                   .= configLog                   conf
        , "replayLog"             .= configReplayLog             conf
        , "stations"              .= configStations              conf
        , "batons"                .= configBatons                conf
        , "rssiThreshold"         .= configRssiThreshold         conf
        , "boxxies"               .= configBoxxies               conf
        , "webPort"               .= configWebPort               conf
        ]

instance FromJSON Config where
    parseJSON (A.Object o) = Config <$>
        o .:? "circuitLength"         .!= configCircuitLength         d <*>
        o .:? "maxSpeed"              .!= configMaxSpeed              d <*>
        o .:? "batonWatchdogLifespan" .!= configBatonWatchdogLifespan d <*>
        o .:? "batonWatchdogInterval" .!= configBatonWatchdogInterval d <*>
        o .:? "sensorPort"            .!= configSensorPort            d <*>
        o .:? "log"                   .!= configLog                   d <*>
        o .:? "replayLog"             .!= configReplayLog             d <*>
        o .:? "stations"              .!= configStations              d <*>
        o .:? "batons"                .!= configBatons                d <*>
        o .:? "rssiThreshold"         .!= configRssiThreshold         d <*>
        o .:? "boxxies"               .!= configBoxxies               d <*>
        o .:? "webPort"               .!= configWebPort               d
      where
        d = defaultConfig

    parseJSON _ = mzero

defaultConfig :: Config
defaultConfig = Config
    { configCircuitLength         = 400
    , configMaxSpeed              = 12  -- 12m/s should be plenty?
    , configBatonWatchdogLifespan = 30
    , configBatonWatchdogInterval = 5
    , configSensorPort            = 9001
    , configLog                   = "log/count-von-count.log"
    , configReplayLog             = "log/replay.log"
    , configStations              = []
    , configBatons                = []
    , configRssiThreshold         = -81
    , configBoxxies               = [defaultBoxxyConfig]
    , configWebPort               = 8000
    }

readConfigFile :: FilePath -> IO Config
readConfigFile filePath = fromMaybe
    (error $ "Could not read config: " ++ filePath) <$> decodeFile filePath
