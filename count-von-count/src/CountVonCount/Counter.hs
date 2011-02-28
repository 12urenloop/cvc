{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CountVonCount.Counter
    ( Counter
    , tick
    ) where

import Data.Monoid (Monoid, mempty)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M

import CountVonCount.Types

newtype Counter = Counter
    { unCounter :: Map Position Timestamp
    } deriving (Show, Monoid)

-- | The last position present in the counter map
--
lastPosition :: Counter -> (Maybe Position)
lastPosition (Counter counts)
    | M.null counts = Nothing 
    | otherwise     = Just $ fst $ M.findMax counts

-- | Check if all times are consecutive
--
consecutive :: Counter -> Bool
consecutive = isSorted . map snd . M.toAscList . unCounter
  where
    -- Check if a list is sorted
    isSorted (x : y : t) = x <= y && isSorted (y : t)
    isSorted _           = True

-- | See a runner at a certain position
--
mark :: Measurement  -- ^ Measurement given
     -> Counter      -- ^ Current state
     -> Counter      -- ^ Updated counter
mark (position, timestamp) (Counter counts) =
    -- Update the counter with the new timestamp, unless a timestamp was already
    -- present, in which case we just keep the current timestamp.
    let updated = fromMaybe timestamp $ M.lookup position counts
    in Counter $ M.insert position updated counts

-- | Check for a lap
--
checkLap :: Measurement  -- ^ Measurement given
         -> Counter      -- ^ Counter state
         -> Maybe Lap    -- ^ Resulting lap
checkLap (position, _) counter =
    case (madeLap, consecutive counter) of
        (False, _)    -> Nothing
        (True, True)  -> Just Lap
        (True, False) -> Just $ SuspiciousLap "what is this shit?"
  where
    madeLap = case lastPosition counter of
        Nothing -> False
        Just l  -> l > position

-- | Someone was seen at a certain position
--
tick :: Measurement           -- ^ Measurement given
     -> Counter               -- ^ Counter to update
     -> (Maybe Lap, Counter)  -- ^ Result, updated state
tick measurement counter =
    case checkLap measurement counter of
        Nothing -> (Nothing, mark measurement counter)
        lap     -> (lap, mark measurement mempty)
