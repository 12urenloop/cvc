-- | Rest API connectivity
--
module CountVonCount.Rest
    ( runRest
    ) where

import CountVonCount.Configuration
import CountVonCount.FiniteChan
import CountVonCount.Types

runRest :: Configuration                        -- ^ Configuration
        -> FiniteChan (Timestamp, Team, Score)  -- ^ Out channel to push
        -> IO ()                                -- ^ Blocks forever
runRest _ chan = runFiniteChan chan () $ \(timestamp, team, score) () -> do
    putStrLn $ show (timestamp, team, score)
