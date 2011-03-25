-- | Remembers measurements
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CountVonCount.DataSet
    ( addMeasurement
    , toSamples
    , toList
    ) where

import qualified Data.Vector.Generic as V

import Statistics.Types (Sample)

import CountVonCount.Types

addMeasurement :: Measurement
               -> DataSet
               -> DataSet
addMeasurement (t, p) (DataSet ts ps) = DataSet (t : ts) (p : ps)

toSamples :: DataSet           -- ^ DataSet
          -> (Sample, Sample)  -- ^ (times, positions)
toSamples (DataSet ts ps) =
    (V.reverse (V.fromList ts), V.reverse (V.fromList ps))

toList :: DataSet                  -- ^ DataSet
       -> [(Timestamp, Position)]  -- ^ Simple list
toList (DataSet ts ps) = reverse $ zip ts ps
