-- | Collection of types used in count-von-count
--
module CountVonCount.Types
    ( Station
    , Position
    , Timestamp
    , Timediff
    , Mac
    , Measurement
    , DataSet (..)
    , Line (..)
    , Criterium
    , Lap (..)
    , Score (..)
    , validateScore
    , Report (..)
    , validateReport
    , Verbosity (..)
    , Logger
    ) where

import Data.Monoid (Monoid, mappend, mempty)

import Data.ByteString (ByteString)

import Statistics.Types (Sample)

-- | A station to which receives information of runners near it
--
type Station = ByteString

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
type Mac = ByteString

-- | A measurement from a station
--
type Measurement = (Timestamp, Position)

-- | Dataset describing a number of measurements
--
data DataSet = DataSet [Timestamp] [Position]
             deriving (Show)

instance Monoid DataSet where
    mempty = DataSet mempty mempty
    DataSet t1 p1 `mappend` DataSet t2 p2 =
        DataSet (mappend t1 t2) (mappend p1 p2)

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

-- | Validate a lap
--
validateScore :: Score -> Bool
validateScore (Refused _) = False
validateScore _           = True

-- | Result of a lap, contains the score and various statistics
--
data Report = Report
    { reportMac        :: Mac
    , reportTimestamp  :: Timestamp
    , reportScore      :: Score
    , reportDataset    :: DataSet
    , reportRegression :: Line
    } deriving (Show)

-- | Validate a report
--
validateReport :: Report -> Bool
validateReport = validateScore . reportScore

-- | Log priority
--
data Verbosity = Debug | Info | Warn | Error
               deriving (Show, Eq, Ord, Read)

-- | Logging structure
--
type Logger = Verbosity -> String -> IO ()
