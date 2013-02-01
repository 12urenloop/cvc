--------------------------------------------------------------------------------
-- | This module is responsible for converting raw sensor data to actually
-- useful data, and discarding irrelevant data
module CountVonCount.Sensor.Filter
    ( SensorEvent (..)
    , filterSensorEvent
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Data.Time                 (UTCTime)


--------------------------------------------------------------------------------
import           CountVonCount.Persistence
import           CountVonCount.Sensor


--------------------------------------------------------------------------------
data SensorEvent = SensorEvent
    { sensorTime    :: UTCTime
    , sensorStation :: Station
    , sensorBaton   :: Baton
    } deriving (Show)


--------------------------------------------------------------------------------
filterSensorEvent :: Database
                  -> Double
                  -> RawSensorEvent
                  -> IO (Maybe SensorEvent)
filterSensorEvent database rssiThreshold raw
    | rawSensorRssi raw < rssiThreshold = return Nothing
    | otherwise                         = do
        mstation <- getStationByMac database (rawSensorStation raw)
        mbaton   <- getBatonByMac database (rawSensorBaton raw)
        case mstation of
            Nothing      -> return Nothing  -- TODO: warning
            Just station -> return $
                SensorEvent (rawSensorTime raw) station <$> mbaton
