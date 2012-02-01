-- | This module is responsible for converting raw sensor data to actually
-- useful data, and discarding irrelevant data
module CountVonCount.Sensor.Filter
    ( SensorEvent (..)
    , filterSensorEvent
    ) where

import Control.Arrow ((&&&))
import qualified Data.Map as M

import Data.Time (UTCTime)

import CountVonCount.Sensor
import CountVonCount.Types

data SensorEvent = SensorEvent
    { sensorTime    :: UTCTime
    , sensorStation :: Station
    , sensorBaton   :: Baton
    } deriving (Show)

filterSensorEvent :: Double
                  -> [Station]
                  -> [Baton]
                  -> RawSensorEvent
                  -> Maybe SensorEvent
filterSensorEvent rssiThreshold stations batons raw
    | rawSensorRssi raw < rssiThreshold = Nothing
    | otherwise                         = do
        station <- M.lookup (rawSensorStation raw) stMap
        baton   <- M.lookup (rawSensorBaton raw) bMap
        return $ SensorEvent (rawSensorTime raw) station baton
  where
    stMap = M.fromList $ fmap (stationMac &&& id) stations
    bMap  = M.fromList $ fmap (batonMac &&& id)   batons
