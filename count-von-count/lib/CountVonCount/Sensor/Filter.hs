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
import           Control.Applicative          (pure, (<$>), (<*>))
import           Data.Foldable                (forM_)
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime)
import           Data.Typeable                (Typeable)
import           Prelude


--------------------------------------------------------------------------------
import           CountVonCount.EventBase      (EventBase)
import qualified CountVonCount.EventBase      as EventBase
import           CountVonCount.Log            (Log)
import qualified CountVonCount.Log            as Log
import           CountVonCount.Persistence
import           CountVonCount.RawSensorEvent


--------------------------------------------------------------------------------
data SensorEvent = SensorEvent
    { sensorTime    :: UTCTime
    , sensorStation :: Station
    , sensorBaton   :: Baton
    , sensorTeam    :: Team
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
        -- We first check if we find a baton. This is an optimization: many
        -- other bluetooth devices will be present at the event and we want to
        -- ignore these as soon as possible.
        mbaton <- getBatonByMac database (rawSensorBaton raw)
        case mbaton of
            Nothing    -> return Nothing
            Just baton -> do
                -- Now we can spend some time to find the station and team.
                mstation <- getStationByMac database (rawSensorStation raw)
                mteam    <- getTeamByMac database (rawSensorBaton raw)
                case mstation of
                    -- An undefined station is something that never should
                    -- happen. If we do encounter this, there's probably a
                    -- configuration error. Hence, we show a warning.
                    Nothing      -> do
                        Log.string logger
                            "CountVonCount.Sensor.Filter.filterSensorEvent" $
                            "Warning: got event from unknown station " ++
                            T.unpack (rawSensorStation raw)
                        return Nothing
                    Just station -> return $
                        SensorEvent (rawSensorTime raw) station baton
                            <$> mteam <*> pure (rawSensorRssi raw)


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
