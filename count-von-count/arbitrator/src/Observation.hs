{-# LANGUAGE DeriveGeneric #-}

module Observation (Observation (..)) where

import Data.Time (UTCTime)
import GHC.Generics
import Data.Aeson
import Data.Text

data Observation = Observation
    { timestamp :: UTCTime
    , team      :: Text
    , location  :: Double
    } deriving (Generic, Show)

instance ToJSON Observation
instance FromJSON Observation
