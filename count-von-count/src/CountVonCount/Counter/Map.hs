--------------------------------------------------------------------------------
-- | Provides an easy structure for multiple counters
{-# LANGUAGE BangPatterns #-}
module CountVonCount.Counter.Map
    ( CounterMap
    , emptyCounterMap
    , stepCounterMap
    , resetCounterMapFor
    , lookupCounterState
    , lastUpdatedBefore
    ) where


--------------------------------------------------------------------------------
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import           Data.Time                   (UTCTime)


--------------------------------------------------------------------------------
import           CountVonCount.Counter.Core
import           CountVonCount.Persistence
import           CountVonCount.Sensor.Filter


--------------------------------------------------------------------------------
type CounterMap = Map (Ref Team) CounterState


--------------------------------------------------------------------------------
emptyCounterMap :: CounterMap
emptyCounterMap = M.empty


--------------------------------------------------------------------------------
stepCounterMap :: Double
               -> Double
               -> SensorEvent
               -> CounterMap
               -> ([CounterCoreEvent], [String], CounterMap)
stepCounterMap circuitLength maxSpeed event !cmap =
    let state                = lookupCounterState teamRef cmap
        app                  = stepCounterState circuitLength maxSpeed event
        (es, tells, !state') = runCounterM app state
    in (es, map prepend tells, M.insert teamRef state' cmap)
  where
    teamRef     = teamId $ sensorTeam event
    prepend str = show (sensorTeam event) ++ ": " ++ str


--------------------------------------------------------------------------------
-- | Resets the counter state for a single baton
resetCounterMapFor :: Ref Team
                   -> CounterMap
                   -> CounterMap
resetCounterMapFor = flip M.insert emptyCounterState


--------------------------------------------------------------------------------
lookupCounterState :: Ref Team
                   -> CounterMap
                   -> CounterState
lookupCounterState teamRef = fromMaybe emptyCounterState . M.lookup teamRef


--------------------------------------------------------------------------------
-- | Get a list of batons which were last updated before the given time
lastUpdatedBefore :: UTCTime -> CounterMap -> [Ref Team]
lastUpdatedBefore time cmap =
    [ teamRef
    | (teamRef, cstate) <- M.toList cmap
    , maybe False (< time) (counterLastUpdate cstate)
    ]
