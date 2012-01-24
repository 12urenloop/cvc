{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Config
    ( Config (..)
    , defaultConfig
    , readConfigFile
    ) where

import Control.Applicative ((<$>))
import Control.Monad (join)

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.ByteString.Char8 as BC
import qualified Data.Object as Yaml
import qualified Data.Object.Yaml as Yaml

import CountVonCount.Types

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

defaultConfig :: Config
defaultConfig = Config
    { configCircuitLength = 400
    , configSensorPort    = 9001
    , configStations      = []
    , configBatons        = []
    }

readConfigFile :: FilePath -> IO Config
readConfigFile filePath = do
    file <- join $ Yaml.decodeFile filePath
    root <- Yaml.fromMapping file

    -- TODO: actually use defaultConfig?
    circuitLength <- read <$> Yaml.lookupScalar "Circuit length" root
    sensorPort    <- read <$> Yaml.lookupScalar "Sensor port" root
    stations <- mapM makeStation =<< Yaml.lookupMapping "Stations" root
    batons   <- mapM makeBaton   =<< Yaml.lookupMapping "Batons" root

    return Config
        { configCircuitLength = circuitLength
        , configSensorPort    = sensorPort
        , configStations      = stations
        , configBatons        = batons
        }
  where
    makeBaton (k, v) = do
        value <- Yaml.fromScalar v
        return $ Baton (BC.pack value) (read k)
    makeStation (k, v) = do
        properties <- Yaml.fromMapping v
        mac <- Yaml.lookupScalar "Mac" properties
        pos <- Yaml.lookupScalar "Position" properties
        return $ Station k (BC.pack mac) (read pos)
