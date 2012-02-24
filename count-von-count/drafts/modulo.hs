import Data.Time (UTCTime, diffUTCTime)
import Data.List (minimumBy)
import Data.Ord (comparing)

import CountVonCount.Sensor.Filter
import CountVonCount.Types

-- | Double-based modulo arithmetic
newtype Modulo = Modulo (Double -> Double)

-- | Convert a double to a double in modulo arithmetic
toModulo :: Double -> Modulo
toModulo = Modulo . modulo

-- | Convert a double in modulo arithmetic to a double in R. The infinite list
-- contains only positive possibilities and starts with the smallest value.
fromModulo :: Modulo -> Double -> [Double]
fromModulo (Modulo f) m = let x = f m in [x, x + m ..]

-- | `mod` for doubles
modulo :: Double -> Double -> Double
modulo x m =
    let i = floor (x / m) :: Int
    in  x - m * fromIntegral i

(.+.) :: Modulo -> Modulo -> Modulo
(.+.) = binop (+)

(.-.) :: Modulo -> Modulo -> Modulo
(.-.) = binop (-)

(.*.) :: Modulo -> Modulo -> Modulo
(.*.) = binop (*)

(./.) :: Modulo -> Modulo -> Modulo
(./.) = binop (/)

binop :: (Double -> Double -> Double) -> Modulo -> Modulo -> Modulo
binop (<>) (Modulo f) (Modulo g) = Modulo $ \m -> modulo (f m <> g m) m

solve :: Double              -- ^ Length of the circuit
      -> Double              -- ^ Absolute maximum speed
      -> Double              -- ^ Previous position
      -> Double              -- ^ Current position
      -> Double              -- ^ Delta time
      -> [(Double, Double)]  -- ^ Possible (delta position, speed) values
solve len max' x1 x2 dt =
    let dxs = fromModulo (toModulo x2 .-. toModulo x1) len
    in [ (dx, sp) | dx <- dxs, let sp = dx / dt, sp > 0, sp < max']

data CounterState
    -- | No data for now
    = NoCounterState
    -- | First, previous event, current speed
    | CounterState SensorEvent SensorEvent Double
    deriving (Show)

stepCounterState :: Double        -- ^ Length of the circuit
                 -> Double        -- ^ Absolute maximum speed
                 -> SensorEvent   -- ^ Incoming event
                 -> CounterState  -- ^ Current state
                 -> CounterState  -- ^ New state
stepCounterState _   _        e NoCounterState = CounterState e e 0
stepCounterState len maxSpeed event (CounterState first prev prevSpeed)
    -- At the same station, do nothing.
    | station == prevStation = CounterState first prev prevSpeed
    -- Refused sensor event
    | null possibilities     = CounterState first prev prevSpeed
    | otherwise              =
        CounterState first event speed'
  where
    SensorEvent time     station     _ = event
    SensorEvent prevTime prevStation _ = prev
    Station _ _ position               = station
    Station _ _ prevPosition           = prevStation

    deltaTime     = time `diffTime` prevTime
    possibilities = solve len maxSpeed prevPosition position deltaTime

    -- Pick the (dx, speed) so that speed close to the previously known speed
    (dx, speed) = minimumBy (comparing $ \(_, sp) -> abs (sp - prevSpeed))
        possibilities

    -- Update exponentially moving average
    speed' = prevSpeed * 0.7 + speed * 0.3

    -- Calculate number of laps to emit (usually 0)
    numLaps = floor $ (prevPosition + dx) / len

    diffTime t1 t2 = fromRational $ toRational $ t1 `diffUTCTime` t2
