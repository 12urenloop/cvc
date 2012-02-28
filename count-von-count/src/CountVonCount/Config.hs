{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Config
    ( Config (..)
    , defaultConfig
    , readConfigFile
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Data.Maybe (fromMaybe)

import Data.Aeson (FromJSON (..), ToJSON (..), (.=), (.:), (.:?), (.!=))
import Data.Text (Text)
import Data.Yaml (decodeFile)
import qualified Data.Aeson as A

import CountVonCount.Types

data BoxxyConfig = BoxxyConfig
    { boxxyHost :: Text
    , boxxyPort :: Int
    , boxxyPath :: Text
    , boxxyKey  :: Text
    } deriving (Show)

instance ToJSON BoxxyConfig where
    toJSON conf = A.object
        [ "host" .= boxxyHost conf
        , "port" .= boxxyPort conf
        , "path" .= boxxyPath conf
        , "key"  .= boxxyKey  conf
        ]

instance FromJSON BoxxyConfig where
    parseJSON (A.Object o) = BoxxyConfig <$>
        o .:? "host" .!= boxxyHost defaultBoxxyConfig <*>
        o .:? "port" .!= boxxyPort defaultBoxxyConfig <*>
        o .:? "path" .!= boxxyPath defaultBoxxyConfig <*>
        o .:? "key"  .!= boxxyKey  defaultBoxxyConfig

    parseJSON _ = mzero

defaultBoxxyConfig :: BoxxyConfig
defaultBoxxyConfig = BoxxyConfig
    { boxxyHost = "localhost"
    , boxxyPort = 80
    , boxxyPath = ""
    , boxxyKey  = "tetten"
    }

data Config = Config
    { configCircuitLength :: Double
    , configMaxSpeed      :: Double
    , configSensorPort    :: Int
    , configLog           :: FilePath
    , configReplayLog     :: FilePath
    , configStations      :: [Station]
    , configBatons        :: [Baton]
    , configRssiThreshold :: Double
    , configBoxxies       :: [BoxxyConfig]
    } deriving (Show)

instance ToJSON Config where
    toJSON conf = A.object
        [ "circuitLength" .= configCircuitLength conf
        , "maxSpeed"      .= configMaxSpeed      conf
        , "sensorPort"    .= configSensorPort    conf
        , "log"           .= configLog           conf
        , "replayLog"     .= configReplayLog     conf
        , "stations"      .= configStations      conf
        , "batons"        .= configBatons        conf
        , "rssiThreshold" .= configRssiThreshold conf
        , "boxxies"       .= configBoxxies       conf
        ]

instance FromJSON Config where
    parseJSON (A.Object o) = Config <$>
        o .:? "circuitLength" .!= configCircuitLength defaultConfig <*>
        o .:? "maxSpeed"      .!= configMaxSpeed      defaultConfig <*>
        o .:? "sensorPort"    .!= configSensorPort    defaultConfig <*>
        o .:? "log"           .!= configLog           defaultConfig <*>
        o .:? "replayLog"     .!= configReplayLog     defaultConfig <*>
        o .:? "stations"      .!= configStations      defaultConfig <*>
        o .:? "batons"        .!= configBatons        defaultConfig <*>
        o .:? "rssiThreshold" .!= configRssiThreshold defaultConfig <*>
        o .:? "boxxies"       .!= configBoxxies       defaultConfig

    parseJSON _ = mzero

defaultConfig :: Config
defaultConfig = Config
    { configCircuitLength = 400
    , configMaxSpeed      = 12  -- 12m/s should be plenty?
    , configSensorPort    = 9001
    , configLog           = "log/count-von-count.log"
    , configReplayLog     = "log/replay.log"
    , configStations      = []
    , configBatons        = []
    , configRssiThreshold = -81
    , configBoxxies       = [defaultBoxxyConfig]
    }

readConfigFile :: FilePath -> IO Config
readConfigFile filePath = fromMaybe
    (error $ "Could not read config: " ++ filePath) <$> decodeFile filePath
