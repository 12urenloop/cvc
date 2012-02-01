-- | This module is responsible for analyzing and filtering stick events
module CountVonCount.Counter.Core
    ( CounterEvent (..)
    , isLap
    , Counter
    , emptyCounter
    , stepCounter
    ) where

import Data.Fixed (mod')
import Data.Time (UTCTime, diffUTCTime)

import CountVonCount.Sensor
import CountVonCount.Types

data CounterEvent
    = Progression UTCTime Station Double
    | Lap UTCTime Double
    deriving (Show)

isLap :: CounterEvent -> Bool
isLap (Lap _ _) = True
isLap _         = False

data Counter = Counter
    { sensorEvents :: [SensorEvent]
    } deriving (Show)

emptyCounter :: Counter
emptyCounter = Counter {sensorEvents = []}

stepCounter :: Double
            -> SensorEvent
            -> Counter
            -> ([CounterEvent], Counter)
stepCounter circuitLength event state
    -- First event received
    | null events             =
        ([], Counter [event])
    -- Still at the same station. Do nothing.
    | station == lastStation  =
        ([], state)
    -- Advanced at least one station, update.
    | position > lastPosition =
        ([Progression time station speed], Counter (event : events))
    -- At a lower position. Either a lap was made, or the sensor event was
    -- foobar. For a lap to be made, we consider a minimum number of stations
    -- and a minimum timespan.
    | falseLap                =
        ([], state)
    -- We have an actual lap!
    | otherwise               =
        ([Progression time station speed, Lap time lapTime], Counter [event])
  where
    Counter events                       = state
    SensorEvent time station _ _         = event
    SensorEvent lastTime lastStation _ _ = head events
    Station _ _ position                 = station
    Station _ _ lastPosition             = lastStation
    SensorEvent lapStart _ _ _           = last events

    lapTime = fromRational $ toRational $
        time `diffUTCTime` lapStart

    falseLap = (lastPosition - position) < minimumDrop ||
        lapTime < minimumLapTime

    speed = ((position - lastPosition) `mod'` circuitLength) /
        (time `diffTime` lastTime)

    minimumDrop    = circuitLength / 2   -- TODO: configurable
    minimumLapTime = 10                  -- TODO: configurable

    diffTime t1 t2 = fromRational $ toRational $ t1 `diffUTCTime` t2
