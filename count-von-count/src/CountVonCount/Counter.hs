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
    , couterStateFor

    , findDeadBatons
    , watchdog
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Foldable (forM_)

import Data.Time (addUTCTime, getCurrentTime)

import CountVonCount.Counter.Core
import CountVonCount.Counter.Map
import CountVonCount.Log (Log)
import CountVonCount.Sensor.Filter
import CountVonCount.Types
import CountVonCount.Util
import qualified CountVonCount.Log as Log
import qualified CountVonCount.Persistence as P

newtype Counter = Counter {unCounter :: MVar CounterMap}

newCounter :: IO Counter
newCounter = Counter <$> newMVar emptyCounterMap

runCounter :: Counter
           -> Double
           -> Double
           -> Log
           -> Handler (P.Team, CounterState, CounterEvent)
           -> Chan SensorEvent
           -> IO ()
runCounter counter cl ms logger handler' chan = forever $ do
    event <- readChan chan
    modifyMVar_ (unCounter counter) (step' event)
  where
    step' = step cl ms logger handler'

step :: Double  -- ^ Circuit length
     -> Double  -- ^ Max speed
     -> Log
     -> Handler (P.Team, CounterState, CounterEvent)
     -> SensorEvent
     -> CounterMap
     -> IO CounterMap
step cl ms logger handler' event cmap = do
    let (events, tells, cmap') = stepCounterMap cl ms event cmap
        cstate                 = lookupCounterState (sensorBaton event) cmap'
    forM_ tells $ Log.string logger
    process cstate events
    return cmap'
  where
    process _      []     = return ()
    process cstate events = isolate_ logger "CounterEvent process" $ do
        mteam  <- P.runPersistence $
            P.getTeamByMac (batonMac . sensorBaton $ event)

        forM_ mteam $ \(ref, team) ->
            forM_ events $ \event' -> do
                -- Add the lap in the database and update team record
                team' <- case event' of
                    Lap timestamp _ -> P.runPersistence $ P.addLap ref timestamp
                    _               -> return team

                -- Call handlers, log
                liftIO $ callHandler logger handler' (team', cstate, event')
                liftIO $ Log.string logger $ case event' of
                    Progression _ s _ -> show team' ++ " @ " ++ show s
                    Lap _ _           -> "Lap for " ++ show team'

resetCounterFor :: Baton -> Counter -> IO ()
resetCounterFor baton counter =
    modifyMVar_ (unCounter counter) $ return . resetCounterMapFor baton

couterStateFor :: Baton -> Counter -> IO CounterState
couterStateFor baton counter =
    lookupCounterState baton <$> readMVar (unCounter counter)

findDeadBatons :: Int -> Counter -> IO [Baton]
findDeadBatons lifespan counter = do
    now  <- getCurrentTime
    cmap <- readMVar (unCounter counter)
    return $ lastUpdatedBefore (negate lifespan' `addUTCTime` now) cmap
  where
    lifespan' = fromInteger $ fromIntegral lifespan

watchdog :: Counter          -- ^ Counter handle
         -> Log              -- ^ Log handle
         -> Int              -- ^ Interval (seconds) to check for dead batons
         -> Int              -- ^ Seconds after a baton is declared dead
         -> Handler [Baton]  -- ^ Dead baton handler
         -> IO ()            -- ^ Blocks forever
watchdog counter logger interval lifespan handler' = forever $ do
    dead <- findDeadBatons lifespan counter
    callHandler logger handler' dead
    threadDelay $ interval * 1000 * 1000
