-- | Various configurable criteria
--
module CountVonCount.Configuration.Criteria
    ( samplesTreshold
    , speedTreshold
    , speedLimit
    , consecutive
    , timeTreshold
    , distanceTreshold
    , loadCriteria
    ) where

import Text.Printf (printf)

import Control.Applicative ((<$>))
import Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as V
import Data.Object (fromMapping, lookupScalar)
import Data.Object.Yaml (YamlObject, fromYamlScalar, toYamlScalar)
import Data.Maybe (catMaybes)

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
-- TODO

-- | Criterium: check that the racer didn't go too slow
--
speedTreshold :: Double -> Criterium
speedTreshold min' _ _ (Line _ speed)
    | speed >= min' = Good
    | otherwise     = Refused $
        printf "Too slow, %f while min is %f" speed min'

-- | Criterium: the racer didn't go too fast
--
speedLimit :: Double -> Criterium
speedLimit max' _ _ (Line _ speed)
    | speed <= max' = Good
    | otherwise     = Refused $
        printf "Impossibly fast, %f while max is %f" speed max'

-- | Criterium: Check that samples are consecutive
--
consecutive :: Criterium
consecutive _ positions _ = isSorted positions
  where
    isSorted xs
        | V.length xs < 2 = Good
        | xs ! 0 > xs ! 1 = Warning ["Unconsecutive samples"]
        | otherwise       = isSorted $ V.tail xs

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

-- | Load the criteria
--
loadCriteria :: YamlObject -> Maybe [Criterium]
loadCriteria object = do
    m <- fromMapping object
    return $ catMaybes
        [ samplesTreshold   <$> lookup' "Samples treshold"  m
        , speedTreshold     <$> lookup' "Speed treshold"    m
        , speedLimit        <$> lookup' "Speed limit"       m
        , const consecutive <$> exists  "Consecutive"       m
        , timeTreshold      <$> lookup' "Time treshold"     m
        , distanceTreshold  <$> lookup' "Distance treshold" m
        ]
  where
    lookup' k m = read . fromYamlScalar <$> lookupScalar (toYamlScalar k) m
    exists k m = const () <$> lookupScalar (toYamlScalar k) m
