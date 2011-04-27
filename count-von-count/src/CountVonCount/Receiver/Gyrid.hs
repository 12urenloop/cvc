-- | Provides communication with gyrid
--
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Receiver.Gyrid
    ( initGyrid
    , parseGyrid
    ) where

import Data.Monoid (mappend)

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

-- | Lines of data that should be sent upon connect
--
initGyrid :: [ByteString]
initGyrid =
    [ "MSG,enable_rssi,true"
    , "MSG,cache,clear"
    ]

-- | Parse a line of gyrid output
--
parseGyrid :: ByteString -> Maybe (ByteString, ByteString)
parseGyrid bs = case SBC.split ',' bs of
    -- Ignore msg & info stuff
    ("MSG" : _)                -> Nothing
    ("INFO" : _)               -> Nothing
    -- In data
    [station, _, mac, _, "in"] -> Just (addColons station, addColons mac)
    -- RSSI data
    [station, _, mac, _]       -> Just (addColons station, addColons mac)
    _                          -> Nothing

-- | Transform a mac without @:@ delimiters to one a mac with @:@ delimiters
--
addColons :: ByteString -> ByteString
addColons bs = if ':' `SBC.elem` bs then bs else addColons' bs
  where
    addColons' bs' = case SB.splitAt 2 bs' of
        (h, "")   -> h
        (h, rest) -> h `mappend` ":" `mappend` addColons rest
