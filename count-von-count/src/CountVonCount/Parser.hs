module CountVonCount.Parser
    ( parse
    ) where

import CountVonCount.Types

parse :: String -> Measurement
parse line =
    let [position, timestamp] = map read $ words line
    in (position, timestamp)
