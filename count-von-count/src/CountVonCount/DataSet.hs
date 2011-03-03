-- | Remembers measurements
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CountVonCount.DataSet
    ( DataSet
    , addMeasurement
    , toSamples
    , toList
    ) where

import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Vector.Generic as V

import Statistics.Types (Sample)

import CountVonCount.Types

data DataSet = DataSet [Timestamp] [Position]

instance Monoid DataSet where
    mempty = DataSet mempty mempty
    DataSet t1 p1 `mappend` DataSet t2 p2 =
        DataSet (mappend t1 t2) (mappend p1 p2)

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
