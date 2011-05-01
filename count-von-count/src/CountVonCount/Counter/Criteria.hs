-- | Various configurable criteria
--
module CountVonCount.Counter.Criteria
    ( samplesTreshold
    , stationsTreshold
    , speedTreshold
    , speedLimit
    , timeTreshold
    , distanceTreshold
    ) where

import Text.Printf (printf)

import qualified Data.Set as S
import Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as V

import CountVonCount.Types

-- | Criterium: we have a significant amount of samples
--
samplesTreshold :: Int -> Criterium
samplesTreshold min' times _ _
    | samples >= min' = Good
    | otherwise       = Refused $
        printf "Not enough samples, got %d, want %d" samples min'
  where
    samples = V.length times

-- | Criterium: the racer was noticed at enough positions
--
stationsTreshold :: Int -> Criterium
stationsTreshold min' _ positions _
    | stations >= min' = Good
    | otherwise        = Refused $
        printf "Not enough stations, got %d, want %d" stations min'
  where
    stations = S.size $ S.fromList $ V.toList positions

-- | Criterium: check that the racer didn't go too slow
--
speedTreshold :: Double -> Criterium
speedTreshold min' _ _ (Line _ speed)
    | speed >= min' = Good
    | otherwise     = Warning $ return $
        printf "Too slow, %f while min is %f" speed min'

-- | Criterium: the racer didn't go too fast
--
speedLimit :: Double -> Criterium
speedLimit max' _ _ (Line _ speed)
    | speed <= max' = Good
    | otherwise     = Refused $
        printf "Impossibly fast, %f while max is %f" speed max'

-- | Criterium: enough time has passed
--
timeTreshold :: Double -> Criterium
timeTreshold min' times _ _
    | V.length times < 1 || timeTaken < min' = Refused $
        printf "Too little time taken, %f while min is %f" timeTaken min'
    | otherwise = Good
  where
    timeTaken = V.last times - times ! 0

-- | Criterium: a great enough distance was travelled
--
distanceTreshold :: Double -> Criterium
distanceTreshold min' _ positions _
    | V.length positions < 1 || distance < min' = Refused $
        printf "Too small distance, %f while min is %f" distance min'
    | otherwise = Good
  where
    distance = V.maximum positions - V.minimum positions
