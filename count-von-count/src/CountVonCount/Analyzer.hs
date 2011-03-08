-- | Analyze a dataset
--
module CountVonCount.Analyzer
    ( analyze
    , regression
    , toGnuPlot
    ) where

import Text.Printf (printf)
import Data.Monoid (mconcat)

import Statistics.Types (Sample)
import Statistics.LinearRegression (linearRegression)

import CountVonCount.Configuration
import CountVonCount.DataSet
import CountVonCount.Types

analyze :: Configuration -> DataSet -> Score
analyze configuration dataSet =
    let (times, positions) = toSamples dataSet
        line = regression times positions
    in criteria times positions line
  where
    criteria = mconcat $ configurationCriteria configuration

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
