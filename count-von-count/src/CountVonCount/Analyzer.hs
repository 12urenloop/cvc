-- | Analyze a dataset
--
module CountVonCount.Analyzer
    ( Line
    , analyze
    , toGnuPlot
    ) where

import Text.Printf (printf)

import Statistics.LinearRegression (linearRegression)

import CountVonCount.DataSet
import CountVonCount.Types

data Line = Line Double Double
          deriving (Show)

analyze :: DataSet -> Line
analyze dataSet =
    let (times, positions) = toSamples dataSet
        (a, b) = linearRegression times positions
    in Line a b

toGnuPlot :: DataSet -> String
toGnuPlot dataSet = unlines $ concat
    [ [ printf "plot %f + %f * x, '-'" a b ]
    , map (uncurry $ printf "%f %f") list
    , [ "e" ]
    ]
  where
    Line a b = analyze dataSet
    list = toList dataSet
