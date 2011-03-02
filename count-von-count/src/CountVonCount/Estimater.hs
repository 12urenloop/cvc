-- | Holds estimations for lap times
--
module CountVonCount.Estimater
    ( Estimater
    , estimate
    , constantEstimater
    ) where

import CountVonCount.Types

-- | Used to estimate lap times
--
newtype Estimater = Estimater
    { unEstimater :: Position -> Position -> Timediff
    }

-- | Make an estimate
--
estimate :: Estimater  -- ^ Used estimater
         -> Position   -- ^ From
         -> Position   -- ^ To
         -> Timediff   -- ^ Expected time
estimate = unEstimater

-- | A constant estimater: expected timediff between all position pairs
--
constantEstimater :: Int        -- ^ Number of positions
                  -> Timediff   -- ^ Timediff between two positions
                  -> Estimater  -- ^ Resulting estimater
constantEstimater positions diff = Estimater $ \from to ->
    ((positions + to - from) `mod` positions) * diff
