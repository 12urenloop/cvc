--------------------------------------------------------------------------------
-- | This module has a number of responsibilities:
--
-- 1. Receiving sensor events
--
-- 2. Grouping the relevant events by stick mac
--
-- 3. Calling the analyzer to process these events
{-# LANGUAGE DeriveDataTypeable #-}
module CountVonCount.Counter
    ( CounterEvent (..)
    , CounterState (..)
    , Counter
    , newCounter
    , subscribe

    , resetCounterFor
    , counterStateFor

    , DeadBatons (..)
    , findDeadBatons
    , watchdog
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.MVar     (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad               (forM, forever)
import           Data.Foldable               (forM_)
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              readIORef, writeIORef)
import           Data.List                   (sortBy)
import           Data.Maybe                  (catMaybes)
import           Data.Time                   (addUTCTime, getCurrentTime)
import           Data.Typeable               (Typeable)
import           Prelude


--------------------------------------------------------------------------------
import           CountVonCount.Counter.Core
import           CountVonCount.Counter.Map
import           CountVonCount.EventBase     (EventBase)
import qualified CountVonCount.EventBase     as EventBase
import           CountVonCount.Log           (Log)
import qualified CountVonCount.Log           as Log
import qualified CountVonCount.Persistence   as P
import           CountVonCount.Sensor.Filter (SensorEvent (..))
import           CountVonCount.Util


--------------------------------------------------------------------------------
data CounterEvent
    = PositionEvent P.Team CounterState
    | LapEvent P.Team P.Lap
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
data Counter = Counter
    { counterLog           :: Log
    , counterDatabase      :: P.Database
    , counterCircuitLength :: Double
    , counterMaxSpeed      :: Double
    , counterStations      :: [P.Station]
    , counterMap           :: IORef CounterMap
    , counterLock          :: MVar ()
    }


--------------------------------------------------------------------------------
newCounter :: Log -> P.Database -> Double -> Double -> IO Counter
newCounter logger database circuitLength maxSpeed = do
    cmap     <- newIORef emptyCounterMap
    mvar     <- newMVar ()
    stations <- P.getAllStations database
    let stations' = sortBy comparePosition stations
    return $ Counter logger database circuitLength maxSpeed stations' cmap mvar
  where
    comparePosition a b = compare (P.stationPosition a) (P.stationPosition b)


--------------------------------------------------------------------------------
subscribe :: Counter
          -> EventBase
          -> IO ()
subscribe counter eventBase =
    EventBase.subscribe eventBase "CountVonCount.Counter.subscribe" $
        step counter eventBase


--------------------------------------------------------------------------------
step :: Counter -> EventBase -> SensorEvent -> IO ()
step counter eventBase event = do
    () <- takeMVar $ counterLock counter
    cmap <- readIORef (counterMap counter)
    let (events, tells, cmap') = stepCounterMap (counterCircuitLength counter)
                                                (counterMaxSpeed counter)
                                                (counterStations counter)
                                                event
                                                cmap
        cstate = lookupCounterState (P.teamId $ sensorTeam event) cmap'
    forM_ tells $ Log.string logger "CountVonCount.Counter.step"
    process cstate events
    writeIORef (counterMap counter) cmap'
    putMVar (counterLock counter) ()
  where
    database = counterDatabase counter
    logger   = counterLog counter

    process _      []     = return ()
    process cstate events = isolate_ logger "CounterEvent process" $ do
        let team = sensorTeam event
        forM_ events $ \event' -> case event' of
            -- Add the lap in the database and update team record
            LapCoreEvent time _ _ -> do
                Log.string logger "CountVonCount.Counter.step" $
                    "Lap for " ++ show team
                lapId' <- P.addLap database (P.teamId team) time
                lap    <- P.getLap database lapId'
                team'  <- P.getTeam database (P.teamId team)
                EventBase.publish eventBase $ LapEvent team' lap

            PositionCoreEvent _ s _ -> do
                team' <- P.getTeam database (P.teamId team)
                Log.string logger "CountVonCount.Counter.step" $
                    show team' ++ " @ " ++ show s
                EventBase.publish eventBase $ PositionEvent team' cstate


--------------------------------------------------------------------------------
resetCounterFor :: P.Ref P.Team -> Counter -> IO ()
resetCounterFor teamRef counter = do
    () <- takeMVar (counterLock counter)
    modifyIORef (counterMap counter) $ resetCounterMapFor teamRef
    putMVar (counterLock counter) ()


--------------------------------------------------------------------------------
counterStateFor :: P.Ref P.Team -> Counter -> IO CounterState
counterStateFor teamRef counter =
    lookupCounterState teamRef <$> readIORef (counterMap counter)


--------------------------------------------------------------------------------
newtype DeadBatons = DeadBatons [(P.Team, P.Baton)]
    deriving (Eq, Show, Typeable)


--------------------------------------------------------------------------------
findDeadBatons :: Int -> Counter -> IO DeadBatons
findDeadBatons lifespan counter = do
    now      <- getCurrentTime
    cmap     <- readIORef $ counterMap counter
    let teamRefs = lastUpdatedBefore (negate lifespan' `addUTCTime` now) cmap
    teams <- fmap catMaybes $ forM teamRefs $ \teamRef -> do
        team  <- P.getTeam (counterDatabase counter) teamRef
        case P.teamBaton team of
            Nothing       -> return Nothing
            Just batonRef -> do
                baton <- P.getBaton (counterDatabase counter) batonRef
                return $ Just (team, baton)
    return $ DeadBatons teams
  where
    lifespan' = fromInteger $ fromIntegral lifespan


--------------------------------------------------------------------------------
watchdog :: Counter                  -- ^ Counter handle
         -> EventBase                -- ^ Event base
         -> Int                      -- ^ Dead baton check interval (seconds)
         -> Int                      -- ^ Seconds after a baton is declared dead
         -> IO ()                    -- ^ Blocks forever
watchdog counter eventBase interval lifespan = forever $ do
    dead <- findDeadBatons lifespan counter
    EventBase.publish eventBase dead
    threadDelay $ interval * 1000 * 1000
