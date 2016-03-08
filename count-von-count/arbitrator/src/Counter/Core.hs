--------------------------------------------------------------------------------
-- | This module implements the counter logic for a /single/ team
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Counter.Core
    ( CounterCoreEvent (..)
    , isLapEvent
    , CounterState (..)
    , emptyCounterState
    , counterLastUpdate
    , CounterM
    , runCounterM
    , stepCounterState
    ) where


--------------------------------------------------------------------------------
import           Control.Monad        (when)
import           Control.Monad.State  (State, get, put, runState)
import           Control.Monad.Writer (WriterT, runWriterT, tell)
import           Data.Aeson
import           Data.List            (minimumBy)
import           Data.Ord             (comparing)
import           Data.Time            (UTCTime, diffUTCTime)
import           Data.Typeable        (Typeable)
import           Text.Printf          (printf)


--------------------------------------------------------------------------------
import           Counter.Modulo
import           Sighting


--------------------------------------------------------------------------------
data CounterCoreEvent
    -- | Time, position, speed
    = PositionCoreEvent UTCTime Double Double
    | LapCoreEvent UTCTime Double
    deriving (Show)


--------------------------------------------------------------------------------
instance ToJSON CounterCoreEvent where
    toJSON (PositionCoreEvent time pos speed) = object
        [ "position" .= object
            [ "timestamp" .= time
            , "position"  .= pos
            , "speed"     .= speed
            ]
        ]
    toJSON (LapCoreEvent time _) = object
        [ "lap" .= object ["timestamp" .= time ]]


--------------------------------------------------------------------------------
isLapEvent :: CounterCoreEvent -> Bool
isLapEvent (LapCoreEvent _ _) = True
isLapEvent _                  = False


--------------------------------------------------------------------------------
data CounterState
    -- | No data for now
    = NoCounterState
    -- | First, previous event, current speed, last seen
    | CounterState Sighting Sighting Double UTCTime
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
emptyCounterState :: CounterState
emptyCounterState = NoCounterState


--------------------------------------------------------------------------------
counterLastUpdate :: CounterState -> Maybe UTCTime
counterLastUpdate NoCounterState         = Nothing
counterLastUpdate (CounterState _ _ _ t) = Just t


--------------------------------------------------------------------------------
type CounterM a = WriterT [String] (State CounterState) a


--------------------------------------------------------------------------------
runCounterM :: CounterM a -> CounterState -> (a, [String], CounterState)
runCounterM c cs =
    let state             = (runWriterT c)
        ((x, tells), cs') = runState state cs
    in (x, tells, cs')


--------------------------------------------------------------------------------
tell' :: String -> CounterM ()
tell' x = tell [x]


--------------------------------------------------------------------------------
stepCounterState :: Double                       -- ^ Absolute maximum speed
                 -> Sighting                     -- ^ Incoming event
                 -> CounterM [CounterCoreEvent]  -- ^ Outgoing events
stepCounterState maxSpeed event = do
    state <- get
    case state of
        NoCounterState               -> do
            put $ CounterState event event 0 (sightingTime event)
            let position = sightingPos event
            tell' $ printf "Initialized counter state (%.2fm)" position
            return []
        CounterState first prev prevSpeed _
            -- At the same station, do nothing.
            | position == prevPosition -> do
                put $ CounterState first prev prevSpeed time
                return []
            -- Refused sensor event
            | null possibilities     -> do
                tell' "Impossibru!"
                return []
            | otherwise              -> do
                tell' $ printf "New position: %.2fm (last: %.2fm, %0fs ago)"
                            position prevPosition dt
                tell' $ printf "Most likely: moved %.2fm at %.2fm/s" dx speed
                tell' $ printf "Updated average speed from %.2fm/s to %.2fm/s"
                            prevSpeed speed'
                when (numLaps > 0) $ tell' $ printf "Adding %d laps" numLaps
                put $ CounterState first event speed' time
                return $
                    -- TODO: lapTime
                    replicate numLaps (LapCoreEvent time 0) ++
                    [PositionCoreEvent time position speed]
          where
            Sighting time     _ position     = event
            Sighting prevTime _ prevPosition = prev

            dt            = time `diffTime` prevTime
            possibilities = solve maxSpeed prevPosition position dt

            -- Pick the (dx, speed) so that speed close to the previously
            -- known speed
            {-
            (dx, speed) = minimumBy
                (comparing $ \(_, sp) -> abs (sp - prevSpeed))
                possibilities
            -}
            -- This allows a team to take a nap and score some extra points in
            -- the meanwhile. Hence, we just take the minimum speed for noew.
            (dx, speed) = minimumBy (comparing snd) possibilities

            -- Update exponentially moving average
            speed' = prevSpeed * 0.7 + speed * 0.3

            -- Calculate number of laps to emit (usually 0)
            numLaps :: Int
            numLaps = floor $ (prevPosition + dx)

            diffTime t1 t2 = fromRational $ toRational $ t1 `diffUTCTime` t2


--------------------------------------------------------------------------------
solve :: Double              -- ^ Absolute maximum speed
      -> Double              -- ^ Previous position
      -> Double              -- ^ Current position
      -> Double              -- ^ Delta time
      -> [(Double, Double)]  -- ^ Possible (delta position, speed) values
solve max' x1 x2 dt = takeWhile ((< max') . snd)
    [(dx, sp) | dx <- dxs, let sp = dx / dt, sp > 0]
  where
    dxs = fromModulo (toModulo x2 .-. toModulo x1) 1
