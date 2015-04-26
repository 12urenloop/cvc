--------------------------------------------------------------------------------
module CountVonCount.Sensor.Replay
    ( subscribe
    ) where


--------------------------------------------------------------------------------
import           Data.List                   (intercalate)
import qualified Data.Text                   as T
import           Data.Time                   (formatTime, defaultTimeLocale)


--------------------------------------------------------------------------------
import           CountVonCount.EventBase     (EventBase)
import qualified CountVonCount.EventBase     as EventBase
import qualified CountVonCount.Log           as Log
import           CountVonCount.Persistence
import           CountVonCount.Sensor.Filter (SensorEvent (..))


--------------------------------------------------------------------------------
-- | Listen to raw sensor events and write them to the replay log
subscribe :: EventBase  -- ^ EventBase to subscribe to
          -> FilePath   -- ^ File path to replay log
          -> IO ()      -- ^ Returns immediately
subscribe eventBase replayLogFile = do
    replayLog <- Log.open replayLogFile False
    Log.raw replayLog $ "\"Time\",\"Station ID\",\"Station Mac\"," ++
        "\"Baton ID\",\"Baton Mac\",\"Team ID\",\"RSSI\""
    EventBase.subscribe eventBase "CountVonCount.Sensor.Replay.subscribe" $
        \event -> Log.raw replayLog $ toReplay event


--------------------------------------------------------------------------------
-- | Format a 'SensorEvent' in order to be readable by the replay log
toReplay :: SensorEvent -> String
toReplay (SensorEvent time station baton team rssi) = intercalate ","
    [ formatTime defaultTimeLocale "%H:%M:%S" time
    , show (stationId station)
    , T.unpack (stationMac station)
    , show (batonId baton)
    , T.unpack (batonMac baton)
    , show (teamId team)
    , show rssi
    ]
