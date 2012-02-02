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

newtype Counter = Counter {unCounter :: MVar CounterMap}

newCounter :: IO Counter
newCounter = Counter <$> newMVar emptyCounterMap

runCounter :: Counter
           -> Double
           -> Log
           -> (P.Team -> CounterEvent -> IO ())
           -> Chan SensorEvent
           -> IO ()
runCounter counter cl logger handler chan = forever $ do
    event <- readChan chan
    modifyMVar_ (unCounter counter) (step' event)
  where
    step' = step cl logger handler

step :: Double
     -> Log
     -> (P.Team -> CounterEvent -> IO ())
     -> SensorEvent
     -> CounterMap
     -> IO CounterMap
step cl logger handler event cmap = do
    let (events, cmap') = stepCounterMap cl event cmap
    process events
    return cmap'
  where
    process []     = return ()
    process events = P.runPersistence $ do
        mteam <- P.getTeamByMac (batonMac . sensorBaton $ event)
        forM_ mteam $ \(ref, team) ->
            forM_ events $ \event' -> do
                liftIO $ do
                    handler team event'
                    Log.string logger $ case event' of
                        Progression _ s _ -> show team ++ " @ " ++ show s
                        Lap _ _           -> "Lap for " ++ show team

                -- Add the lap in the database
                case event' of
                    Lap timestamp _ -> P.addLap ref timestamp
                    _               -> return ()

resetCounterFor :: Baton -> Counter -> IO ()
resetCounterFor baton counter =
    modifyMVar_ (unCounter counter) $ return . resetCounterMapFor baton
