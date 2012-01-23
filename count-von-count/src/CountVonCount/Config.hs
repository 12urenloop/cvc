module CountVonCount.Config
    ( Config (..)
    , defaultConfig
    , readConfigFile
    ) where

import Control.Applicative ((<$>))
import Control.Monad (join)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Object as Yaml
import qualified Data.Object.Yaml as Yaml

import CountVonCount.Types

data Config = Config
    { configSensorPort :: Int
    , configStations   :: [Station]
    , configBatons     :: [Baton]
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { configSensorPort = 9001
    , configStations   = []
    , configBatons     = []
    }

readConfigFile :: FilePath -> IO Config
readConfigFile filePath = do
    file <- join $ Yaml.decodeFile filePath
    root <- Yaml.fromMapping file

    -- TODO: actually use defaultConfig?
    sensorPort <- read <$> Yaml.lookupScalar "Sensor port" root
    stations <- mapM makeStation =<< Yaml.lookupMapping "Stations" root
    batons   <- mapM makeBaton   =<< Yaml.lookupMapping "Batons" root

    return Config
        { configSensorPort = sensorPort
        , configStations   = stations
        , configBatons     = batons
        }
  where
    makeBaton (k, v) = do
        value <- Yaml.fromScalar v
        return $ Baton (BC.pack k) (read value)
    makeStation (k, v) = do
        properties <- Yaml.fromMapping v
        mac <- Yaml.lookupScalar "Mac" properties
        pos <- Yaml.lookupScalar "Position" properties
        return $ Station (BC.pack mac) k (read pos)
