-- | Remembers measurements
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CountVonCount.DataSet
    ( emptyDataSet
    , addMeasurement
    , toSamples
    , toList
    ) where

import qualified Data.Vector.Generic as V

import Statistics.Types (Sample)

import CountVonCount.Types

emptyDataSet :: DataSet
emptyDataSet = DataSet [] [] Nothing

addMeasurement :: Measurement
               -> DataSet
               -> DataSet
addMeasurement (t, p) (DataSet ts ps m) = DataSet (t : ts) (p : ps) max'
  where
    max' = Just $ case m of Nothing  -> p
                            Just  m' -> max m' p

toSamples :: DataSet           -- ^ DataSet
          -> (Sample, Sample)  -- ^ (times, positions)
toSamples (DataSet ts ps _) =
    (V.reverse (V.fromList ts), V.reverse (V.fromList ps))

toList :: DataSet                  -- ^ DataSet
       -> [(Timestamp, Position)]  -- ^ Simple list
toList (DataSet ts ps _) = reverse $ zip ts ps
