{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Config
    ( Config (..)
    , defaultConfig
    , readConfigFile
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import CountVonCount.Types
import Data.Maybe (fromJust)
import Data.Yaml

data Config = Config
    { configCircuitLength :: Double
    , configSensorPort    :: Int
    , configStations      :: [Station]
    , configBatons        :: [Baton]
    } deriving (Show)

instance ToJSON Config where
    toJSON conf = object
        [ "circuitLength" .= configCircuitLength conf
        , "sensorPort"    .= configSensorPort conf
        , "stations"      .= configStations conf
        , "batons"        .= configBatons conf
        ]

instance FromJSON Config where
    parseJSON (Object o) = Config <$>
                           o .:? "circuitLength" .!= configCircuitLength defaultConfig <*>
                           o .:? "sensorPort" .!= configSensorPort defaultConfig <*>
                           o .:? "stations" .!= configStations defaultConfig <*>
                           o .:? "batons" .!= configBatons defaultConfig

    parseJSON _ = mzero

defaultConfig :: Config
defaultConfig = Config
    { configCircuitLength = 400
    , configSensorPort    = 9001
    , configStations      = []
    , configBatons        = []
    }

readConfigFile :: FilePath -> IO Config
readConfigFile filePath = fromJust <$> decodeFile filePath
