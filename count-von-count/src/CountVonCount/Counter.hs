-- | This module has a number of responsibilities:
--
-- 1. Receiving sensor events
--
-- 2. Grouping the relevant events by stick mac
--
-- 3. Calling the analyzer to process these events
-- 
module CountVonCount.Counter
    ( counter
    ) where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.Time (UTCTime)
import qualified Data.Map as M

import CountVonCount.Config
import CountVonCount.Counter.Core
import CountVonCount.Counter.Map
import CountVonCount.Types

counter :: Monad m
        => Config
        -> (Mac -> CounterEvent -> m ())  -- TODO: should be: Team -> ...
        -> UTCTime
        -> Mac
        -> Mac
        -> CounterMap
        -> m CounterMap
counter conf handler = loop
  where
    loop time smac bmac cmap
        | ignoreMac bmac = return cmap
        | otherwise      = do
            case M.lookup smac stationMap of
                Nothing      -> return cmap
                Just station -> do
                    let sensorEvent     = SensorEvent time station
                        (events, cmap') = stepCounterMap bmac sensorEvent cmap
                    forM_ events $ handler smac 
                    return cmap'

    ignoreMac  = const False  -- TODO
    stationMap = M.fromList $ map (stationMac &&& id) $ configStations conf
