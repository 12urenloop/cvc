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
    , configStations      :: [Station]
    , configBatons        :: [Baton]
    } deriving (Show)

instance ToJSON Config where
    toJSON conf = A.object
        [ "circuitLength" .= configCircuitLength conf
        , "sensorPort"    .= configSensorPort    conf
        , "stations"      .= configStations      conf
        , "batons"        .= configBatons        conf
        ]

instance FromJSON Config where
    parseJSON (A.Object o) = Config <$>
        o .:? "circuitLength" .!= configCircuitLength defaultConfig <*>
        o .:? "sensorPort"    .!= configSensorPort    defaultConfig <*>
        o .:? "stations"      .!= configStations      defaultConfig <*>
        o .:? "batons"        .!= configBatons        defaultConfig

    parseJSON _ = mzero

defaultConfig :: Config
defaultConfig = Config
    { configCircuitLength = 400
    , configSensorPort    = 9001
    , configStations      = []
    , configBatons        = []
    }

readConfigFile :: FilePath -> IO Config
readConfigFile filePath = fromMaybe
    (error $ "Could not read config: " ++ filePath) <$> decodeFile filePath
