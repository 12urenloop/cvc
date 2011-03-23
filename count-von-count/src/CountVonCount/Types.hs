-- | Collection of types used in count-von-count
--
module CountVonCount.Types
    ( Station
    , Position
    , Timestamp
    , Timediff
    , Mac
    , Measurement
    , Line (..)
    , Criterium
    , Lap (..)
    , Score (..)
    , Logger
    ) where

import Data.Monoid (Monoid, mappend, mempty)

import Statistics.Types (Sample)

import Control.DeepSeq (NFData (..))

-- | A station to which receives information of runners near it
--
type Station = String

-- | The position of a 'Station'
--
type Position = Double

-- | Represents a time
--
type Timestamp = Double

-- | A difference between two 'Timestamp's
--
type Timediff = Double

-- | Uniquely identify a mac address
--
type Mac = String

-- | A measurement from a station
--
type Measurement = (Timestamp, Position)

-- | A line specified by offset and steepness
--
data Line = Line Double Double
          deriving (Show)

-- | Returns @Nothing@ if all is OK, @Just xxx@ with a descriptive error
--
type Criterium = Sample -> Sample -> Line -> Score

-- | Description of a lap
--
data Lap = Lap
         | SuspiciousLap String
         deriving (Show, Eq)

instance NFData Lap where
    rnf Lap                 = ()
    rnf (SuspiciousLap str) = rnf str

-- | A score given to a lap
--
data Score = Good
           | Warning [String]
           | Refused String
           deriving (Show, Eq)

instance Monoid Score where
    mempty = Good
    mappend (Refused x) _           = Refused x
    mappend _           (Refused y) = Refused y
    mappend (Warning x) (Warning y) = Warning (x ++ y)
    mappend (Warning x) _           = Warning x
    mappend _           (Warning y) = Warning y
    mappend Good        Good        = Good

instance NFData Score where
    rnf Good         = ()
    rnf (Warning ss) = rnf ss
    rnf (Refused s)  = rnf s

-- | Logging structure
--
type Logger = String -> IO ()
