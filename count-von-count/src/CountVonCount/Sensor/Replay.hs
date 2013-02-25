--------------------------------------------------------------------------------
module CountVonCount.Sensor.Replay
    ( subscribe
    ) where


--------------------------------------------------------------------------------
import           Data.List               (intercalate)
import qualified Data.Text               as T
import           Data.Time               (formatTime)
import           System.Locale           (defaultTimeLocale)


--------------------------------------------------------------------------------
import           CountVonCount.EventBase (EventBase)
import qualified CountVonCount.EventBase as EventBase
import qualified CountVonCount.Log       as Log
import           CountVonCount.Sensor


--------------------------------------------------------------------------------
-- | Listen to raw sensor events and write them to the replay log
subscribe :: EventBase  -- ^ EventBase to subscribe to
          -> FilePath   -- ^ File path to replay log
          -> IO ()      -- ^ Returns immediately
subscribe eventBase replayLogFile = do
    replayLog <- Log.open replayLogFile False
    EventBase.subscribe eventBase "CountVonCount.Sensor.Replay.subscribe" $
        \event -> Log.raw replayLog $ toReplay event


--------------------------------------------------------------------------------
-- | Format a 'SensorEvent' in order to be readable by the replay log
toReplay :: RawSensorEvent -> String
toReplay (RawSensorEvent time station baton rssi) = intercalate ","
    [ "REPLAY"
    , formatTime defaultTimeLocale "%s" time
    , T.unpack station
    , T.unpack baton
    , show rssi
    ]
