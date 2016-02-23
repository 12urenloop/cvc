{-# LANGUAGE DeriveGeneric #-}

module Observation (Observation (..)) where

import Data.Time (UTCTime)
import GHC.Generics
import Data.Aeson
import Data.Text

type Mac = Text

data Observation = Observation
    { timestamp :: UTCTime
    , station   :: Mac
    , baton     :: Mac
    } deriving (Generic, Show)

instance ToJSON Observation
