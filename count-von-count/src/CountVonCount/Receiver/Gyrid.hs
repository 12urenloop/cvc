-- | Provides communication with gyrid
--
{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module CountVonCount.Receiver.Gyrid
    ( initGyrid
    , parseGyrid
    ) where

import Data.Monoid (mappend)

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC

import CountVonCount.Types
import CountVonCount.Configuration.StationMap

-- | Lines of data that should be sent upon connect
--
initGyrid :: [ByteString]
initGyrid =
    [ "MSG,enable_rssi,true"
    , "MSG,enable_cache,false"
    ]

-- | Parse a line of gyrid output
--
parseGyrid :: StationMap -> Timestamp -> ByteString -> Maybe Command
parseGyrid stationMap timestamp bs = case SBC.split ',' bs of
    -- Ignore msg & info stuff
    ("MSG" : _)  -> Nothing
    ("INFO" : _) -> Nothing

    -- In/RSSI data
    [!station, _, !mac, _, "in"] -> measurement station mac
    [!station, _, !mac, _]       -> measurement station mac

    _ -> Nothing
  where
    measurement :: ByteString -> ByteString -> Maybe Command
    measurement station mac = do
        !pos <- mapStation stationMap $ addColons station
        return $ Measurement $ (addColons mac, (timestamp, pos))

-- | Transform a mac without @:@ delimiters to one a mac with @:@ delimiters
--
addColons :: ByteString -> ByteString
addColons bs = if ':' `SBC.elem` bs then bs else addColons' bs
  where
    addColons' bs' = case SB.splitAt 2 bs' of
        (h, "")   -> h
        (h, rest) -> h `mappend` ":" `mappend` addColons rest
