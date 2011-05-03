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
import CountVonCount.Configuration

-- | Lines of data that should be sent upon connect
--
initGyrid :: [ByteString]
initGyrid =
    [ "MSG,enable_rssi,true"
    , "MSG,enable_cache,false"
    ]

-- | Parse a line of gyrid output
--
parseGyrid :: Configuration -> Timestamp -> ByteString -> Maybe Command
parseGyrid conf timestamp bs = case SBC.split ',' bs of
    -- Ignore msg & info stuff
    ("MSG" : _)  -> Nothing
    ("INFO" : _) -> Nothing

    -- RSSI data
    [!station, _, !mac, !rssi] -> measurement station mac rssi

    -- Replay
    ["REPLAY", !timestamp', !station, _, !mac, !rssi] ->
        replay timestamp' station mac rssi

    _ -> Nothing
  where
    measurement :: ByteString -> ByteString -> ByteString -> Maybe Command
    measurement station mac rssi = do
        !pos <- stationPosition (addColons station) conf
        return $ Measurement $ (addColons mac, (timestamp, pos, readBS rssi))

    -- Replay: replace timestamp
    replay timestamp' s m r = do
        Measurement (mac, (_, pos, rssi)) <- measurement s m r
        return $ Measurement (mac, (readBS timestamp', pos, rssi))

    readBS :: Read a => ByteString -> a
    readBS = read . SBC.unpack

-- | Transform a mac without @:@ delimiters to one a mac with @:@ delimiters
--
addColons :: ByteString -> ByteString
addColons bs = if ':' `SBC.elem` bs then bs else addColons' bs
  where
    addColons' bs' = case SB.splitAt 2 bs' of
        (h, "")   -> h
        (h, rest) -> h `mappend` ":" `mappend` addColons rest
