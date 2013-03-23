--------------------------------------------------------------------------------
-- | This module is responsible for converting raw sensor data to actually
-- useful data, and discarding irrelevant data
{-# LANGUAGE DeriveDataTypeable #-}
module CountVonCount.Sensor.Filter
    ( SensorEvent (..)
    , filterSensorEvent
    , subscribe
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       (pure, (<$>), (<*>))
import           Data.Foldable             (forM_)
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime)
import           Data.Typeable             (Typeable)


--------------------------------------------------------------------------------
import           CountVonCount.EventBase   (EventBase)
import qualified CountVonCount.EventBase   as EventBase
import           CountVonCount.Log         (Log)
import qualified CountVonCount.Log         as Log
import           CountVonCount.Persistence
import           CountVonCount.Sensor


--------------------------------------------------------------------------------
data SensorEvent = SensorEvent
    { sensorTime    :: UTCTime
    , sensorStation :: Station
    , sensorBaton   :: Baton
    , sensorRssi    :: Double
    } deriving (Eq, Show, Typeable)


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
            Just station -> return $ SensorEvent (rawSensorTime raw) station
                    <$> mbaton
                    <*> pure (rawSensorRssi raw)


--------------------------------------------------------------------------------
subscribe :: EventBase
          -> Database
          -> Log
          -> Double
          -> IO ()
subscribe eventBase database logger rssiThreshold = do
    EventBase.subscribe eventBase "CountVonCount.Sensor.Filter.subscribe" $
        \event -> do
            filtered <- filterSensorEvent' event
            forM_ filtered $ EventBase.publish eventBase
  where
    filterSensorEvent' = filterSensorEvent database logger rssiThreshold
