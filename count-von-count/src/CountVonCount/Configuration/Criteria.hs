-- | Various configurable criteria
--
{-# LANGUAGE FlexibleContexts #-}
module CountVonCount.Configuration.Criteria
    ( loadCriteria
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

import Control.Failure (Failure)
import Data.Object (fromMapping, ObjectExtractError)
import Data.Object.Yaml (YamlObject)

import CountVonCount.Types
import CountVonCount.Configuration.Util
import CountVonCount.Counter.Criteria

-- | Load the criteria
--
loadCriteria :: Failure ObjectExtractError m
             => YamlObject -> m [Criterium]
loadCriteria object = do
    m <- fromMapping object
    return $ catMaybes
        [ samplesTreshold   <$> lookup' "Samples treshold"  m
        , stationsTreshold  <$> lookup' "Stations treshold" m
        , speedTreshold     <$> lookup' "Speed treshold"    m
        , speedLimit        <$> lookup' "Speed limit"       m
        , timeTreshold      <$> lookup' "Time treshold"     m
        , distanceTreshold  <$> lookup' "Distance treshold" m
        ]
  where
    lookup' k = fmap read . lookupString k
