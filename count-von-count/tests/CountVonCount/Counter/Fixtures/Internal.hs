-- | A simple DSL for counter testing
module CountVonCount.Counter.Fixtures.Internal
    ( CounterFixtureM
    , runCounterFixtureM
    , CounterFixture (..)
    , noLap
    , lap
    , sensorEvents
    , numLaps
    ) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (Writer, execWriter, tell)

import Data.Time (UTCTime, addUTCTime)

import CountVonCount.Persistence
import CountVonCount.Sensor.Filter

type CounterFixtureM = ReaderT (UTCTime, Team, Baton) (Writer [CounterFixture])

runCounterFixtureM :: CounterFixtureM () -> UTCTime -> Team -> Baton
                   -> [CounterFixture]
runCounterFixtureM cf time team baton =
    execWriter $ runReaderT cf (time, team, baton)

data CounterFixture = CounterFixture SensorEvent Bool
    deriving (Show)

noLap :: Int -> Station -> CounterFixtureM ()
noLap = at False

lap :: Int -> Station -> CounterFixtureM ()
lap = at True

at :: Bool -> Int -> Station -> CounterFixtureM ()
at isLap time station = do
    (offset, team, baton) <- ask
    let time' = fromIntegral time `addUTCTime` offset
    tell [CounterFixture (SensorEvent time' station baton team 0) isLap]

sensorEvents :: [CounterFixture] -> [SensorEvent]
sensorEvents = map (\(CounterFixture se _) -> se)

numLaps :: [CounterFixture] -> Int
numLaps = sum . map (\(CounterFixture _ l) -> if l then 1 else 0)
