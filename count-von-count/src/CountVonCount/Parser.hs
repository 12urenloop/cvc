module CountVonCount.Parser
    ( parse
    ) where

import CountVonCount.Types

parse :: String -> (Team, Measurement)
parse line =
    let [team, position, timestamp] = words line
    in (team, (read position, read timestamp))
