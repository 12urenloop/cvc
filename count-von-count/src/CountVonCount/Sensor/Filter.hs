--------------------------------------------------------------------------------
-- | This module is responsible for converting raw sensor data to actually
-- useful data, and discarding irrelevant data
module CountVonCount.Sensor.Filter
    ( SensorEvent (..)
    , filterSensorEvent
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime)


--------------------------------------------------------------------------------
import           CountVonCount.Log         (Log)
import qualified CountVonCount.Log         as Log
import           CountVonCount.Persistence
import           CountVonCount.Sensor


--------------------------------------------------------------------------------
data SensorEvent = SensorEvent
    { sensorTime    :: UTCTime
    , sensorStation :: Station
    , sensorBaton   :: Baton
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
filterSensorEvent :: Database
                  -> Log
                  -> Double
                  -> RawSensorEvent
                  -> IO (Maybe SensorEvent)
filterSensorEvent database logger rssiThreshold raw
    | rawSensorRssi raw < rssiThreshold = return Nothing
    | otherwise                         = do
        mstation <- getStationByMac database (rawSensorStation raw)
        mbaton   <- getBatonByMac database (rawSensorBaton raw)
        case mstation of
            Nothing      -> do
                Log.string logger
                    "CountVonCount.Sensor.Filter.filterSensorEvent" $
                    "Warning: got event from unknown station " ++
                    T.unpack (rawSensorStation raw)
                return Nothing
            Just station -> return $
                SensorEvent (rawSensorTime raw) station <$> mbaton
