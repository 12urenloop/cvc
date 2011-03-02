-- | Analyze a dataset
--
module CountVonCount.Analyzer
    ( Line
    , analyze
    , regression
    , toGnuPlot
    ) where

import Text.Printf (printf)
import Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as V
import Data.Monoid (mconcat)

import Statistics.Types (Sample)
import Statistics.LinearRegression (linearRegression)

import CountVonCount.DataSet
import CountVonCount.Types

data Line = Line Double Double
          deriving (Show)

-- | Returns @Nothing@ if all is OK, @Just xxx@ with a descriptive error
--
type Criterium = Sample -> Sample -> Line -> Score

-- | Criterium: we have a significant amount of samples
--
enoughSamples :: Int -> Criterium
enoughSamples min' times _ _
    | samples >= min' = Good
    | otherwise       = Refused $
        printf "Not enough samples, got %d, want %d" samples min'
  where
    samples = V.length times

-- | Criterium: the racer was noticed at enough positions
--
-- TODO

-- | Criterium: the racer didn't go too fast
--
speedLimit :: Double -> Criterium
speedLimit max' _ _ (Line _ speed)
    | speed <= max' = Good
    | otherwise     = Refused $
        printf "Impossibly fast, %f while max is %f" speed max'

-- | Check that samples are consecutive
--
consecutive :: Criterium
consecutive _ positions _ = isSorted positions
  where
    isSorted xs
        | V.length xs < 2 = Good
        | xs ! 0 > xs ! 1 = Warning ["Unconsecutive samples"]
        | otherwise       = isSorted $ V.tail xs

-- | Criterium: not too many outliers
--
-- TODO

analyze :: DataSet -> Score
analyze dataSet =
    let (times, positions) = toSamples dataSet
        line = regression times positions
    in (mconcat criteria) times positions line
  where
    -- Used criteria
    criteria = [ enoughSamples 3
               , speedLimit 5
               , consecutive
               ]

regression :: Sample -> Sample -> Line
regression times positions =
    let (a, b) = linearRegression times positions
    in Line a b

toGnuPlot :: DataSet -> String
toGnuPlot dataSet = unlines $ concat
    [ [ printf "plot %f + %f * x, '-'" a b ]
    , map (uncurry $ printf "%f %f") list
    , [ "e" ]
    ]
  where
    Line a b = uncurry regression $ toSamples dataSet
    list = toList dataSet
