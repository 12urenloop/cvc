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
    , counterStateForTeam

    , findDeadBatons
    , watchdog
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative         ((<$>), (<*>))
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.MVar     (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad               (forever)
import           Data.Foldable               (forM_)
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              readIORef, writeIORef)
import           Data.Time                   (addUTCTime, getCurrentTime)
import           Data.Typeable               (Typeable)


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
    { counterMap  :: IORef CounterMap
    , counterLock :: MVar ()
    }


--------------------------------------------------------------------------------
newCounter :: IO Counter
newCounter = Counter <$> newIORef emptyCounterMap <*> newMVar ()


--------------------------------------------------------------------------------
subscribe :: Counter
          -> Double
          -> Double
          -> Log
          -> EventBase
          -> P.Database
          -> IO ()
subscribe counter cl ms logger eventBase db =
    EventBase.subscribe eventBase "CountVonCount.Counter.subscribe" $
        \event -> do
            () <- takeMVar $ counterLock counter
            writeIORef (counterMap counter)
                =<< step cl ms logger eventBase db event
                =<< readIORef (counterMap counter)
            putMVar (counterLock counter) ()


--------------------------------------------------------------------------------
step :: Double  -- ^ Circuit length
     -> Double  -- ^ Max speed
     -> Log
     -> EventBase
     -> P.Database
     -> SensorEvent
     -> CounterMap
     -> IO CounterMap
step cl ms logger eventBase db event cmap = do
    let (events, tells, cmap') = stepCounterMap cl ms event cmap
        cstate = lookupCounterState (P.batonId $ sensorBaton event) cmap'
    forM_ tells $ Log.string logger "CountVonCount.Counter.step"
    process cstate events
    return cmap'
  where
    process _      []     = return ()
    process cstate events = isolate_ logger "CounterEvent process" $ do
        mteam <- P.getTeamByMac db (P.batonMac . sensorBaton $ event)

        -- If the baton is registred to a team
        forM_ mteam $ \team ->
            forM_ events $ \event' -> case event' of
                -- Add the lap in the database and update team record
                LapCoreEvent time _ -> do
                    Log.string logger "CountVonCount.Counter.step" $
                        "Lap for " ++ show team
                    lapId' <- P.addLap db (P.teamId team) time
                    lap    <- P.getLap db lapId'
                    team'  <- P.getTeam db (P.teamId team)
                    EventBase.publish eventBase $ LapEvent team' lap

                PositionCoreEvent _ s _ -> do
                    Log.string logger "CountVonCount.Counter.step" $
                        show team ++ " @ " ++ show s
                    EventBase.publish eventBase $ PositionEvent team cstate


--------------------------------------------------------------------------------
resetCounterFor :: P.Ref P.Baton -> Counter -> IO ()
resetCounterFor baton counter = do
    () <- takeMVar (counterLock counter)
    modifyIORef (counterMap counter) $ resetCounterMapFor baton
    putMVar (counterLock counter) ()


--------------------------------------------------------------------------------
counterStateFor :: P.Ref P.Baton -> Counter -> IO CounterState
counterStateFor baton counter =
    lookupCounterState baton <$> readIORef (counterMap counter)


--------------------------------------------------------------------------------
counterStateForTeam :: P.Team -> Counter -> IO CounterState
counterStateForTeam team counter = case P.teamBaton team of
    Nothing   -> return NoCounterState
    Just bref -> counterStateFor bref counter


--------------------------------------------------------------------------------
findDeadBatons :: Int -> Counter -> IO [P.Ref P.Baton]
findDeadBatons lifespan counter = do
    now  <- getCurrentTime
    cmap <- readIORef $ counterMap counter
    return $ lastUpdatedBefore (negate lifespan' `addUTCTime` now) cmap
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
