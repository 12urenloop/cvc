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
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.Chan (Chan, readChan)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Foldable (forM_)

import CountVonCount.Counter.Core
import CountVonCount.Counter.Map
import CountVonCount.Log (Log)
import CountVonCount.Types
import qualified CountVonCount.Log as Log
import qualified CountVonCount.Persistence as P

import CountVonCount.Sensor.Filter
import CountVonCount.Util

newtype Counter = Counter {unCounter :: MVar CounterMap}

newCounter :: IO Counter
newCounter = Counter <$> newMVar emptyCounterMap

runCounter :: Counter
           -> Double
           -> Double
           -> Log
           -> (P.Team -> CounterEvent -> IO ())
           -> Chan SensorEvent
           -> IO ()
runCounter counter cl ms logger handler chan = forever $ do
    event <- readChan chan
    modifyMVar_ (unCounter counter) (step' event)
  where
    step' = step cl ms logger handler

step :: Double  -- ^ Circuit length
     -> Double  -- ^ Max speed
     -> Log
     -> (P.Team -> CounterEvent -> IO ())
     -> SensorEvent
     -> CounterMap
     -> IO CounterMap
step cl ms logger handler event cmap = do
    let (events, tells, cmap') = stepCounterMap cl ms event cmap
    forM_ tells $ Log.string logger
    process events
    return cmap'
  where
    process []     = return ()
    process events = isolate logger "CounterEvent process" $ do
        mteam <- P.runPersistence $
            P.getTeamByMac (batonMac . sensorBaton $ event)

        forM_ mteam $ \(ref, team) ->
            forM_ events $ \event' -> do
                liftIO $ isolate logger "CounterEvent handler" $ do
                    handler team event'
                    Log.string logger $ case event' of
                        Progression _ s _ -> show team ++ " @ " ++ show s
                        Lap _ _           -> "Lap for " ++ show team

                -- Add the lap in the database
                case event' of
                    Lap timestamp _ -> P.runPersistence $ P.addLap ref timestamp
                    _               -> return ()

resetCounterFor :: Baton -> Counter -> IO ()
resetCounterFor baton counter =
    modifyMVar_ (unCounter counter) $ return . resetCounterMapFor baton
