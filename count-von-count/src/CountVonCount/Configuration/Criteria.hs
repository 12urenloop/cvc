-- | Various configurable criteria
--
module CountVonCount.Configuration.Criteria
    ( loadCriteria
    ) where

import Control.Applicative ((<$>))
import Data.Object (fromMapping)
import Data.Object.Yaml (YamlObject)
import Data.Maybe (catMaybes)

import CountVonCount.Types
import CountVonCount.Configuration.Util
import CountVonCount.Counter.Criteria

-- | Load the criteria
--
loadCriteria :: YamlObject -> Maybe [Criterium]
loadCriteria object = do
    m <- fromMapping object
    return $ catMaybes
        [ samplesTreshold   <$> lookup' "Samples treshold"  m
        , speedTreshold     <$> lookup' "Speed treshold"    m
        , speedLimit        <$> lookup' "Speed limit"       m
        , timeTreshold      <$> lookup' "Time treshold"     m
        , distanceTreshold  <$> lookup' "Distance treshold" m
        ]
  where
    lookup' k = fmap read . lookupString k
