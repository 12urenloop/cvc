module CountVonCount.Types
    ( Position
    , Timestamp
    , Measurement
    , Team
    , Lap (..)
    ) where

import Control.DeepSeq (NFData (..))

type Position = Int
type Timestamp = Int
type Team = String

type Measurement = (Position, Timestamp)

data Lap = Lap
         | SuspiciousLap String
         deriving (Show, Eq)

instance NFData Lap where
    rnf Lap                 = ()
    rnf (SuspiciousLap str) = rnf str
