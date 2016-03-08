--------------------------------------------------------------------------------
-- | Provides an easy structure for multiple counters
{-# LANGUAGE BangPatterns #-}
module Counter.Map
    ( CounterMap
    , emptyCounterMap
    , stepCounterMap
    , resetCounterMapFor
    , lookupCounterState
    , lastUpdatedBefore
    ) where


--------------------------------------------------------------------------------
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Maybe   (fromMaybe)
import           Data.Text    (Text)
import           Data.Time    (UTCTime)


--------------------------------------------------------------------------------
import           Counter.Core
import           Sighting


--------------------------------------------------------------------------------
type CounterMap = Map Text CounterState


--------------------------------------------------------------------------------
emptyCounterMap :: CounterMap
emptyCounterMap = M.empty


--------------------------------------------------------------------------------
stepCounterMap :: Double
               -> Sighting
               -> CounterMap
               -> ([CounterCoreEvent], [String], CounterMap)
stepCounterMap maxSpeed event !cmap =
    let state                = lookupCounterState team cmap
        app                  = stepCounterState maxSpeed event
        (es, tells, !state') = runCounterM app state
    in (es, map prepend tells, M.insert team state' cmap)
  where
    team        = sightingTeam event
    prepend str = show team ++ ": " ++ str


--------------------------------------------------------------------------------
-- | Resets the counter state for a single baton
resetCounterMapFor :: Text
                   -> CounterMap
                   -> CounterMap
resetCounterMapFor = flip M.insert emptyCounterState


--------------------------------------------------------------------------------
lookupCounterState :: Text
                   -> CounterMap
                   -> CounterState
lookupCounterState teamRef = fromMaybe emptyCounterState . M.lookup teamRef


--------------------------------------------------------------------------------
-- | Get a list of batons which were last updated before the given time
lastUpdatedBefore :: UTCTime -> CounterMap -> [Text]
lastUpdatedBefore time cmap =
    [ teamRef
    | (teamRef, cstate) <- M.toList cmap
    , maybe False (< time) (counterLastUpdate cstate)
    ]
