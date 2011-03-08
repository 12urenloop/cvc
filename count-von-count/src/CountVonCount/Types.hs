module CountVonCount.Types
    ( Station
    , Position
    , Timestamp
    , Timediff
    , Team
    , Sensor
    , Measurement
    , Line (..)
    , Criterium
    , Lap (..)
    , Score (..)
    ) where

import Data.Monoid (Monoid, mappend, mempty)

import Statistics.Types (Sample)

import Control.DeepSeq (NFData (..))

type Station = String
type Position = Double
type Timestamp = Double
type Timediff = Double
type Team = String
type Sensor = String

type Measurement = (Timestamp, Position)

data Line = Line Double Double
          deriving (Show)

-- | Returns @Nothing@ if all is OK, @Just xxx@ with a descriptive error
--
type Criterium = Sample -> Sample -> Line -> Score

data Lap = Lap
         | SuspiciousLap String
         deriving (Show, Eq)

instance NFData Lap where
    rnf Lap                 = ()
    rnf (SuspiciousLap str) = rnf str

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
