-- | Receiving input
--
module CountVonCount.Receiver
    ( Receiver
    ) where

import CountVonCount.Types
import CountVonCount.FiniteChan

-- | Gets a channel to write to, and then blocks until the input ends. The
-- receiver should then close the channel.
--
type Receiver = FiniteChan (Team, Measurement) -> IO ()
