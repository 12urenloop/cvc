--------------------------------------------------------------------------------
-- | Communication with boxxy
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CountVonCount.Boxxy
    ( -- * Configuration
      BoxxyConfig (..)
    , defaultBoxxyConfig

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
import           Control.Monad               (forM, forM_, void, when)
import           Data.Aeson                  ((.=))
import qualified Data.Aeson                  as A
import qualified Data.Conduit                as C
import           Data.IORef                  (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe                  (isNothing)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time                   (UTCTime)
import qualified Network.HTTP.Conduit        as Http


--------------------------------------------------------------------------------
import           CountVonCount.Config
import qualified CountVonCount.Counter       as Counter
import           CountVonCount.EventBase
import           CountVonCount.Log           (Log)
import qualified CountVonCount.Log           as Log
import qualified CountVonCount.Persistence   as P
import           CountVonCount.Sensor.Filter (SensorEvent(..))
import           CountVonCount.Util


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
data BoxxyState = Up | Down
    deriving (Eq, Show)


--------------------------------------------------------------------------------
data Boxxies = Boxxies
    { boxxiesConfig   :: Config
    , boxxiesState    :: [(BoxxyConfig, IORef BoxxyState)]
    , boxxiesLog      :: Log
    , boxxiesDatabase :: P.Database
    , boxxiesCounter  :: Counter.Counter
    }


--------------------------------------------------------------------------------
newBoxxies :: Config -> Log -> P.Database -> Counter.Counter -> EventBase
           -> IO Boxxies
newBoxxies config logger database counter eventBase = do
    -- Create boxxies value
    bs <- Boxxies
        <$> pure config
        <*> forM (configBoxxies config) (\c -> (,) c <$> newIORef Down)
        <*> pure logger
        <*> pure database
        <*> pure counter

    -- Try to initialize right away
    withBoxxies bs (const $ return ())

    -- Subscribe to counter events
    subscribe eventBase "boxxies counter handler" $ \counterEvent ->
        withBoxxies bs $ \b -> case counterEvent of
            Counter.LapEvent team lap         -> addLap b lap team
            Counter.PositionEvent team cstate ->
                let Counter.CounterState _ sensorEvent _ timestamp = cstate
                in updatePosition b team (sensorStation sensorEvent) timestamp

    return bs


--------------------------------------------------------------------------------
withBoxxies :: Boxxies
            -> (BoxxyConfig -> IO ())
            -> IO ()
withBoxxies bs f = forM_ (boxxiesState bs) $ \(c, rs) ->
    void $ forkIO $ do
        s <- readIORef rs
        Log.string logger "CountVonCount.Boxxy.withBoxxies" $
            "Calling " ++ show c ++ ", currently " ++ show s
        -- Try to init if needed
        r <- case s of
            Up   -> return Nothing
            Down -> isolate logger ("init " ++ show c) $ do
                ping c
                putState bs (Just c)

        -- Make the call if up
        r' <- case r of
            Nothing -> isolate logger ("call " ++ show c) $ f c
            Just _  -> return r

        let s' = if isNothing r' then Up else Down
        when (s /= s') $ Log.string logger "CountVonCount.Boxxy.withBoxxies" $
            show c ++ " is now " ++ show s'
        writeIORef rs s'
  where
    logger = boxxiesLog bs


--------------------------------------------------------------------------------
ping :: BoxxyConfig -> IO ()
ping config = makeRequest config "/ping" $ A.object []


--------------------------------------------------------------------------------
putState :: Boxxies -> Maybe BoxxyConfig -> IO ()
putState boxxies target = do
    -- Get info
    stations <- P.getAllStations database
    teams    <- P.getAllTeams database
    teams'   <- forM teams $ \team -> do
        cstate <- Counter.counterStateForTeam team (boxxiesCounter boxxies)
        return (team, cstate)
    laps     <- P.getLatestLaps database Nothing 10
    laps'    <- forM laps $ \lap -> do
        team <- P.getTeam database $ P.lapTeam lap
        return (lap, team)

    let withTarget = case target of
            Nothing -> withBoxxies boxxies
            Just t  -> forM_ [t]

    withTarget $ \c -> makeRequest c "/state" $
        stateJson circuitLength stations teams' laps'
  where
    circuitLength = configCircuitLength config
    config        = boxxiesConfig boxxies
    database      = boxxiesDatabase boxxies


--------------------------------------------------------------------------------
addLap :: BoxxyConfig   -- ^ Boxxy instance to notify
       -> P.Lap         -- ^ Lap
       -> P.Team        -- ^ Applicable team
       -> IO ()
addLap config lap team = makeRequest config "/lap" $ lapJson lap team


--------------------------------------------------------------------------------
updatePosition :: BoxxyConfig -> P.Team -> P.Station -> UTCTime -> IO ()
updatePosition config team station timestamp = makeRequest config "/position" $
    positionJson team station timestamp


--------------------------------------------------------------------------------
boxxiesToList :: Boxxies -> IO [(BoxxyConfig, BoxxyState)]
boxxiesToList boxxies = forM (boxxiesState boxxies) $ \(c, rs) -> do
    s <- readIORef rs
    return (c, s)


--------------------------------------------------------------------------------
stateJson :: Double -> [P.Station] -> [(P.Team, Counter.CounterState)]
          -> [(P.Lap, P.Team)] -> A.Value
stateJson circuitLength stations teams laps = A.object
    [ "circuitLength" .= circuitLength
    , "stations" .= A.object
        [ P.refToText (P.stationId s) .= stationJson s
        | s <- stations
        ]
    , "teams" .= A.object
        [P.refToText (P.teamId t) .= teamJson t cs | (t, cs) <- teams]
    , "laps" .= map (uncurry lapJson) laps
    ]


--------------------------------------------------------------------------------
teamJson :: P.Team -> Counter.CounterState -> A.Value
teamJson t cstate = A.object
    [ "id"      .= P.refToText (P.teamId t)
    , "laps"    .= P.teamLaps t
    , "name"    .= P.teamName t
    , "station" .= pos
    , "updated" .= lu
    ]
  where
    (pos, lu) = case cstate of
        Counter.NoCounterState          -> (Nothing, Nothing)
        Counter.CounterState _ se _ lu' ->
            (Just (P.refToText $ P.stationId $ sensorStation se), Just lu')


--------------------------------------------------------------------------------
lapJson :: P.Lap -> P.Team -> A.Value
lapJson lap team = A.object
    [ "count"     .= P.lapCount lap
    , "id"        .= P.refToText (P.lapId lap)
    , "reason"    .= P.lapReason lap
    , "team"      .= P.refToText (P.lapTeam lap)
    , "timestamp" .= P.lapTimestamp lap
    , "total"     .= P.teamLaps team
    ]


--------------------------------------------------------------------------------
stationJson :: P.Station -> A.Value
stationJson station = A.object
    [ "id"       .= P.refToText (P.stationId station)
    , "name"     .= P.stationName station
    , "position" .= P.stationPosition station
    ]


--------------------------------------------------------------------------------
positionJson :: P.Team -> P.Station -> UTCTime -> A.Value
positionJson team station timestamp = A.object
    [ "team"      .= P.refToText (P.teamId team)
    , "station"   .= P.refToText (P.stationId station)
    , "timestamp" .= timestamp
    ]
