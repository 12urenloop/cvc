module CountVonCount.Types
    ( Position
    , Timestamp
    , Timediff
    , Measurement
    , Team
    , Sensor
    , Lap (..)
    , Score (..)
    ) where

import Data.Monoid (Monoid, mappend, mempty)

import Control.DeepSeq (NFData (..))

type Position = Double
type Timestamp = Double
type Timediff = Double
type Team = String
type Sensor = String

type Measurement = (Timestamp, Position)

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
