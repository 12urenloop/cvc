{-# LANGUAGE OverloadedStrings #-}

module Sighting (Sighting (..)) where

import           Control.Monad
import           Data.Aeson
import           Data.Text
import           Data.Time     (UTCTime)


data Sighting = Sighting
    { sightingTime :: UTCTime
    , sightingTeam :: Text
    , sightingPos  :: Double
    } deriving (Show)


instance FromJSON Sighting where
    parseJSON (Object o) = Sighting
        <$> o .: "timestamp"
        <*> o .: "team"
        <*> o .: "position"
    parseJSON _ = mzero


instance ToJSON Sighting where
    toJSON (Sighting time team pos) = object
        [ "timestamp".= time
        , "team"     .= team
        , "position" .= pos
        ]
