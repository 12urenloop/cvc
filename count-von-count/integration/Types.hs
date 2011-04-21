module Types
    ( Count
    ) where

import Data.Map (Map)

import Data.ByteString (ByteString)

type Count = Map ByteString Int
