--------------------------------------------------------------------------------
-- | This module has a number of responsibilities:
--
-- 1. Receiving sensor events
--
-- 2. Grouping the relevant events by stick mac
--
-- 3. Calling the analyzer to process these events
--
module CountVonCount.Counter
    ( Counter
    , newCounter
    , runCounter

    , resetCounterFor
    , counterStateFor

    , findDeadBatons
    , watchdog
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative         ((<$>))
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.Chan     (Chan, readChan)
import           Control.Concurrent.MVar     (MVar, modifyMVar_, newMVar,
                                              readMVar)
import           Control.Monad               (forever)
import           Data.Foldable               (forM_)
import           Data.Time                   (addUTCTime, getCurrentTime)


--------------------------------------------------------------------------------
import           CountVonCount.Counter.Core
import           CountVonCount.Counter.Map
import           CountVonCount.EventBase
import           CountVonCount.Log           (Log)
import qualified CountVonCount.Log           as Log
import qualified CountVonCount.Persistence   as P
import           CountVonCount.Sensor.Filter
import           CountVonCount.Util


--------------------------------------------------------------------------------
newtype Counter = Counter {unCounter :: MVar CounterMap}


--------------------------------------------------------------------------------
newCounter :: IO Counter
newCounter = Counter <$> newMVar emptyCounterMap


--------------------------------------------------------------------------------
runCounter :: Counter
           -> Double
           -> Double
           -> Log
           -> EventBase
           -> P.Database
           -> Chan SensorEvent
           -> IO ()
runCounter counter cl ms logger eventBase db chan = forever $ do
    event <- readChan chan
    modifyMVar_ (unCounter counter) (step' event)
  where
    step' = step cl ms logger eventBase db


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
            forM_ events $ \event' -> do
                -- Add the lap in the database and update team record
                team' <- case event' of
                    Lap timestamp _ -> P.addLap db (P.teamId team) timestamp
                    _               -> return team

                -- Call handlers, log
                publish eventBase (team', cstate, event')
                Log.string logger "CountVonCount.Counter.step" $ case event' of
                    Progression _ s _ -> show team' ++ " @ " ++ show s
                    Lap _ _           -> "Lap for " ++ show team'


--------------------------------------------------------------------------------
resetCounterFor :: P.Ref P.Baton -> Counter -> IO ()
resetCounterFor baton counter =
    modifyMVar_ (unCounter counter) $ return . resetCounterMapFor baton


--------------------------------------------------------------------------------
counterStateFor :: P.Ref P.Baton -> Counter -> IO CounterState
counterStateFor baton counter =
    lookupCounterState baton <$> readMVar (unCounter counter)


--------------------------------------------------------------------------------
findDeadBatons :: Int -> Counter -> IO [P.Ref P.Baton]
findDeadBatons lifespan counter = do
    now  <- getCurrentTime
    cmap <- readMVar (unCounter counter)
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
    publish eventBase dead
    threadDelay $ interval * 1000 * 1000
