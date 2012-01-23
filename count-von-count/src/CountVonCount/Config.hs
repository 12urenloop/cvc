module CountVonCount.Config
    ( Config (..)
    , defaultConfig
    , readConfigFile
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM, join)

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

    sensorPort <- read <$> Yaml.lookupScalar "Sensor port" root

    stations <- map makeStation <$> fromMapping "Stations" root
    batons   <- map makeBaton   <$> fromMapping "Batons"   root

    return Config
        { configSensorPort = sensorPort
        , configStations   = stations
        , configBatons     = batons
        }
  where
    makeStation (k, v) = Station (BC.pack k) (read v)
    makeBaton   (k, v) = Baton (BC.pack k) (read v)

fromMapping :: (Yaml.IsYamlScalar v)
            => String
            -> [(String, Yaml.Object String v)]
            -> IO [(String, v)]
fromMapping name root = do
    pairs <- Yaml.lookupMapping name root
    forM pairs $ \(k, v) -> do
        v' <- Yaml.fromScalar v
        return (k, v')
