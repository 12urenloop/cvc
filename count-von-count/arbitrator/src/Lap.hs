{-# LANGUAGE DeriveGeneric #-}

module Lap (Lap (..)) where

import Data.Time (UTCTime)
import GHC.Generics
import Data.Aeson
import Data.Text

data Lap = Lap
    { timestamp :: UTCTime
    , team      :: Text
    } deriving (Generic, Show)

instance ToJSON Lap
