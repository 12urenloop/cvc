-- | This module implements the counter logic for a /single/ team
module CountVonCount.Counter.Core
    ( CounterEvent (..)
    , isLap
    , CounterState
    , emptyCounterState
    , stepCounterState
    ) where

import Data.Fixed (mod')
import Data.Time (UTCTime, diffUTCTime)

import CountVonCount.Sensor.Filter
import CountVonCount.Types

data CounterEvent
    = Progression UTCTime Station Double
    | Lap UTCTime Double
    deriving (Show)

isLap :: CounterEvent -> Bool
isLap (Lap _ _) = True
isLap _         = False

data CounterState = CounterState
    { sensorEvents :: [SensorEvent]
    } deriving (Show)

emptyCounterState :: CounterState
emptyCounterState = CounterState {sensorEvents = []}

stepCounterState :: Double
                 -> SensorEvent
                 -> CounterState
                 -> ([CounterEvent], CounterState)
stepCounterState circuitLength event state
    -- First event received
    | null events             =
        ([], CounterState [event])
    -- Still at the same station. Do nothing.
    | station == lastStation  =
        ([], state)
    -- Advanced at least one station, update.
    | position > lastPosition =
        ([Progression time station speed], CounterState (event : events))
    -- At a lower position. Either a lap was made, or the sensor event was
    -- foobar. For a lap to be made, we consider a minimum number of stations
    -- and a minimum timespan.
    | falseLap                =
        ([], state)
    -- We have an actual lap! Send two events.
    | otherwise               =
        let es = [Progression time station speed, Lap time lapTime]
        in (es, CounterState [event])
  where
    CounterState events                     = state
    SensorEvent time station _         = event
    SensorEvent lastTime lastStation _ = head events
    Station _ _ position               = station
    Station _ _ lastPosition           = lastStation
    SensorEvent lapStart _ _           = last events

    lapTime = fromRational $ toRational $
        time `diffUTCTime` lapStart

    falseLap = (lastPosition - position) < minimumDrop ||
        lapTime < minimumLapTime

    speed = ((position - lastPosition) `mod'` circuitLength) /
        (time `diffTime` lastTime)

    minimumDrop    = circuitLength / 2   -- TODO: configurable
    minimumLapTime = 10                  -- TODO: configurable

    diffTime t1 t2 = fromRational $ toRational $ t1 `diffUTCTime` t2
