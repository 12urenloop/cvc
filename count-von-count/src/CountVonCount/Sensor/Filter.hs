--------------------------------------------------------------------------------
-- | This module is responsible for converting raw sensor data to actually
-- useful data, and discarding irrelevant data
module CountVonCount.Sensor.Filter
    ( SensorEvent (..)
    , filterSensorEvent
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Arrow             ((&&&))
import qualified Data.Map                  as M
import           Data.Time                 (UTCTime)


--------------------------------------------------------------------------------
import           CountVonCount.Persistence
import           CountVonCount.Sensor
import           CountVonCount.Types


--------------------------------------------------------------------------------
data SensorEvent = SensorEvent
    { sensorTime    :: UTCTime
    , sensorStation :: Station
    , sensorBaton   :: Baton
    } deriving (Show)


--------------------------------------------------------------------------------
filterSensorEvent :: Database
                  -> Double
                  -> [Baton]
                  -> RawSensorEvent
                  -> IO (Maybe SensorEvent)
filterSensorEvent database rssiThreshold batons raw
    | rawSensorRssi raw < rssiThreshold = return Nothing
    | otherwise                         = do
        mstation <- getStationByMac database (rawSensorStation raw)
        case mstation of
            Nothing      -> return Nothing  -- TODO: warning
            Just station -> return $
                SensorEvent (rawSensorTime raw) station <$>
                M.lookup (rawSensorBaton raw) bMap
  where
    bMap = M.fromList $ fmap (batonMac &&& id)   batons
