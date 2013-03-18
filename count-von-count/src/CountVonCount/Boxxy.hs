--------------------------------------------------------------------------------
-- | Communication with boxxy
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CountVonCount.Boxxy
    ( -- * Configuration
      BoxxyConfig (..)
    , defaultBoxxyConfig

      -- * Talking to boxxy
    , putState
    , putLap
    , putPosition

      -- * Stateful talking
    , BoxxyState (..)
    , Boxxies
    , newBoxxies
    , withBoxxies
    , boxxiesToList
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative         (pure, (<$>), (<*>))
import           Control.Concurrent          (forkIO)
import           Control.Monad               (forM, forM_, mzero, void, when)
import           Data.Aeson                  (FromJSON(..), (.!=), (.:?), (.=))
import qualified Data.Aeson                  as A
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BC
import qualified Data.Conduit                as C
import           Data.IORef                  (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe                  (isNothing)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time                   (UTCTime)
import qualified Network.HTTP.Conduit        as Http


--------------------------------------------------------------------------------
import qualified CountVonCount.Counter       as Counter
import           CountVonCount.EventBase
import           CountVonCount.Log           (Log)
import qualified CountVonCount.Log           as Log
import           CountVonCount.Persistence
import           CountVonCount.Sensor.Filter (SensorEvent(..))
import           CountVonCount.Util


--------------------------------------------------------------------------------
data BoxxyConfig = BoxxyConfig
    { boxxyHost     :: Text
    , boxxyPort     :: Int
    , boxxyPath     :: Text
    , boxxyUser     :: ByteString
    , boxxyPassword :: ByteString
    }


--------------------------------------------------------------------------------
instance Show BoxxyConfig where
    show (BoxxyConfig host port path user password) =
        T.unpack host ++ ":" ++ show port ++ "/" ++ T.unpack path ++
        " (" ++ BC.unpack user ++ ":" ++ BC.unpack password ++ ")"


--------------------------------------------------------------------------------
instance FromJSON BoxxyConfig where
    parseJSON (A.Object o) = BoxxyConfig <$>
        o .:? "host"     .!= boxxyHost     defaultBoxxyConfig <*>
        o .:? "port"     .!= boxxyPort     defaultBoxxyConfig <*>
        o .:? "path"     .!= boxxyPath     defaultBoxxyConfig <*>
        o .:? "user"     .!= boxxyUser     defaultBoxxyConfig <*>
        o .:? "password" .!= boxxyPassword defaultBoxxyConfig

    parseJSON _ = mzero


--------------------------------------------------------------------------------
defaultBoxxyConfig :: BoxxyConfig
defaultBoxxyConfig = BoxxyConfig
    { boxxyHost     = "localhost"
    , boxxyPort     = 80
    , boxxyPath     = ""
    , boxxyUser     = "count-von-count"
    , boxxyPassword = "tetten"
    }


--------------------------------------------------------------------------------
makeRequest :: BoxxyConfig -> Text -> A.Value -> IO ()
makeRequest config path body = do
    let rq = Http.applyBasicAuth (boxxyUser config) (boxxyPassword config) $
                Http.def
                    { Http.method         = "PUT"
                    , Http.host           = T.encodeUtf8 (boxxyHost config)
                    , Http.port           = boxxyPort config
                    , Http.path           = T.encodeUtf8 path'
                    , Http.requestBody    = Http.RequestBodyLBS (A.encode body)
                    , Http.requestHeaders =
                        [ ("Connection", "Close")
                        , ("Content-Type", "application/json")
                        ]
                    }

    manager <- Http.newManager Http.def
    _       <- C.runResourceT $ Http.httpLbs rq manager
    Http.closeManager manager
  where
    path' = boxxyPath config `T.append` path


--------------------------------------------------------------------------------
putState :: BoxxyConfig -> Double -> [Station] -> [Team] -> [(Lap, Team)]
         -> IO ()
putState config circuitLength stations teams laps =
    makeRequest config "/state" $ stateJson circuitLength stations teams laps


--------------------------------------------------------------------------------
putLap :: BoxxyConfig   -- ^ Boxxy instance to notify
       -> Lap           -- ^ Lap
       -> Team          -- ^ Applicable team
       -> IO ()
putLap config lap team = makeRequest config "/lap" $ lapJson lap team


--------------------------------------------------------------------------------
putPosition :: BoxxyConfig -> Team -> Station -> UTCTime -> IO ()
putPosition config team station timestamp = makeRequest config "/position" $
    positionJson team station timestamp


--------------------------------------------------------------------------------
data BoxxyState = Up | Down
    deriving (Eq, Show)


--------------------------------------------------------------------------------
data Boxxies = Boxxies
    { boxxiesState :: [(BoxxyConfig, IORef BoxxyState)]
    , boxxiesInit  :: BoxxyConfig -> IO ()
    }


--------------------------------------------------------------------------------
newBoxxies :: Log -> EventBase -> [BoxxyConfig] -> (BoxxyConfig -> IO ())
           -> IO Boxxies
newBoxxies logger eventBase configs ini = do
    bs <- Boxxies <$> forM configs (\c -> (,) c <$> newIORef Down) <*> pure ini
    withBoxxies logger bs (const $ return ())

    -- Subscribe to counter events
    subscribe eventBase "boxxies counter handler" $ \counterEvent ->
        withBoxxies logger bs $ \b -> case counterEvent of
            Counter.LapEvent team lap         -> putLap b lap team
            Counter.PositionEvent team cstate ->
                let Counter.CounterState _ sensorEvent _ timestamp = cstate
                in putPosition b team (sensorStation sensorEvent) timestamp

    return bs


--------------------------------------------------------------------------------
withBoxxies :: Log
            -> Boxxies
            -> (BoxxyConfig -> IO ())
            -> IO ()
withBoxxies logger bs f = forM_ (boxxiesState bs) $ \(c, rs) ->
    void $ forkIO $ do
        s <- readIORef rs
        Log.string logger "CountVonCount.Boxxy.withBoxxies" $
            "Calling " ++ show c ++ ", currently " ++ show s
        -- Try to init if needed
        r <- case s of
            Down -> isolate logger ("init " ++ show c) $ boxxiesInit bs c
            Up   -> return Nothing

        -- Make the call if up
        r' <- case r of
            Nothing -> isolate logger ("call " ++ show c) $ f c
            Just _  -> return r

        let s' = if isNothing r' then Up else Down
        when (s /= s') $ Log.string logger "CountVonCount.Boxxy.withBoxxies" $
            show c ++ " is now " ++ show s'
        writeIORef rs s'


--------------------------------------------------------------------------------
boxxiesToList :: Boxxies -> IO [(BoxxyConfig, BoxxyState)]
boxxiesToList boxxies = forM (boxxiesState boxxies) $ \(c, rs) -> do
    s <- readIORef rs
    return (c, s)


--------------------------------------------------------------------------------
stateJson :: Double -> [Station] -> [Team] -> [(Lap, Team)] -> A.Value
stateJson circuitLength stations teams laps = A.object
    [ "circuitLength" .= circuitLength
    , "stations" .= A.object
        [ refToText (stationId s) .= stationJson s
        | s <- stations
        ]
    , "teams" .= A.object
        [refToText (teamId t) .= teamJson t | t <- teams]
    , "laps" .= map (uncurry lapJson) laps
    ]


--------------------------------------------------------------------------------
teamJson :: Team -> A.Value
teamJson t = A.object
    [ "id"   .= refToText (teamId t)
    , "laps" .= teamLaps t
    , "name" .= teamName t
    ]


--------------------------------------------------------------------------------
lapJson :: Lap -> Team -> A.Value
lapJson lap team = A.object
    [ "count"     .= lapCount lap
    , "id"        .= refToText (lapId lap)
    , "reason"    .= lapReason lap
    , "team"      .= refToText (lapTeam lap)
    , "timestamp" .= lapTimestamp lap
    , "total"     .= teamLaps team
    ]


--------------------------------------------------------------------------------
stationJson :: Station -> A.Value
stationJson station = A.object
    [ "id"       .= refToText (stationId station)
    , "name"     .= stationName station
    , "position" .= stationPosition station
    ]


--------------------------------------------------------------------------------
positionJson :: Team -> Station -> UTCTime -> A.Value
positionJson team station timestamp = A.object
    [ "team"      .= refToText (teamId team)
    , "station"   .= refToText (stationId station)
    , "timestamp" .= timestamp
    ]
