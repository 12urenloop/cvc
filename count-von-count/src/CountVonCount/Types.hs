module CountVonCount.Types
    ( Position
    , Timestamp
    , Measurement
    , Team
    , Sensor
    , Lap (..)
    ) where

import Control.DeepSeq (NFData (..))

type Position = Double
type Timestamp = Double
type Team = String
type Sensor = String

type Measurement = (Timestamp, Position)

data Lap = Lap
         | SuspiciousLap String
         deriving (Show, Eq)

instance NFData Lap where
    rnf Lap                 = ()
    rnf (SuspiciousLap str) = rnf str
