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
import Data.Time (UTCTime (..))

import Data.Aeson (FromJSON (..), ToJSON (..), (.=), (.:?), (.!=))
import Data.Yaml (decodeFile)
import qualified Data.Aeson as A

import CountVonCount.Boxxy

data Config = Config
    { configStartTime             :: UTCTime
    , configCircuitLength         :: Double
    , configMaxSpeed              :: Double
    , configBatonWatchdogLifespan :: Int
    , configBatonWatchdogInterval :: Int
    , configSensorPort            :: Int
    , configLog                   :: FilePath
    , configReplayLog             :: FilePath
    , configRssiThreshold         :: Double
    , configBoxxies               :: [BoxxyConfig]
    , configWebPort               :: Int
    } deriving (Show)

instance ToJSON Config where
    toJSON conf = A.object
        [ "startTime"             .= configStartTime             conf
        , "circuitLength"         .= configCircuitLength         conf
        , "maxSpeed"              .= configMaxSpeed              conf
        , "batonWatchdogLifespan" .= configBatonWatchdogLifespan conf
        , "batonWatchdogInterval" .= configBatonWatchdogInterval conf
        , "sensorPort"            .= configSensorPort            conf
        , "log"                   .= configLog                   conf
        , "replayLog"             .= configReplayLog             conf
        , "rssiThreshold"         .= configRssiThreshold         conf
        , "boxxies"               .= configBoxxies               conf
        , "webPort"               .= configWebPort               conf
        ]

instance FromJSON Config where
    parseJSON (A.Object o) = Config <$>
        o .:? "startTime"             .!= configStartTime             d <*>
        o .:? "circuitLength"         .!= configCircuitLength         d <*>
        o .:? "maxSpeed"              .!= configMaxSpeed              d <*>
        o .:? "batonWatchdogLifespan" .!= configBatonWatchdogLifespan d <*>
        o .:? "batonWatchdogInterval" .!= configBatonWatchdogInterval d <*>
        o .:? "sensorPort"            .!= configSensorPort            d <*>
        o .:? "log"                   .!= configLog                   d <*>
        o .:? "replayLog"             .!= configReplayLog             d <*>
        o .:? "rssiThreshold"         .!= configRssiThreshold         d <*>
        o .:? "boxxies"               .!= configBoxxies               d <*>
        o .:? "webPort"               .!= configWebPort               d
      where
        d = defaultConfig

    parseJSON _ = mzero

defaultConfig :: Config
defaultConfig = Config
    { configStartTime             = UTCTime (toEnum 0) 0
    , configCircuitLength         = 400
    , configMaxSpeed              = 12  -- 12m/s should be plenty?
    , configBatonWatchdogLifespan = 30
    , configBatonWatchdogInterval = 5
    , configSensorPort            = 9001
    , configLog                   = "log/count-von-count.log"
    , configReplayLog             = "log/replay.log"
    , configRssiThreshold         = -81
    , configBoxxies               = [defaultBoxxyConfig]
    , configWebPort               = 8000
    }

readConfigFile :: FilePath -> IO Config
readConfigFile filePath = fromMaybe
    (error $ "Could not read config: " ++ filePath) <$> decodeFile filePath
