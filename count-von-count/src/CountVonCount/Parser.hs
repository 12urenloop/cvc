module CountVonCount.Parser
    ( parse
    ) where

import CountVonCount.Types

parse :: String -> (Team, Measurement)
parse line =
    let [team, timestamp, position] = words line
    in (team, (read timestamp, read position))
