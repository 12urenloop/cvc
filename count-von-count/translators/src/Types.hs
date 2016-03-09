--------------------------------------------------------------------------------
-- | TODO: Move this module or give it a better name
{-# LANGUAGE OverloadedStrings #-}
module Types
    ( Mac
    , parseMac
    , parseHexMac
    ) where


--------------------------------------------------------------------------------
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as BC
import           Data.ByteString.Lazy.Builder.ASCII (lazyByteStringHexFixed)
import           Data.ByteString.Lazy.Builder       (toLazyByteString)
import qualified Data.ByteString.Lazy               as BL
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as T


--------------------------------------------------------------------------------
type Mac = Text


--------------------------------------------------------------------------------
-- | Transform a mac without @:@ delimiters to one a mac with @:@ delimiters
parseMac :: ByteString -> Mac
parseMac bs = T.decodeUtf8 $ if ':' `BC.elem` bs then bs else parseMac' bs
  where
    parseMac' bs' = case BC.splitAt 2 bs' of
        (h, "")   -> h
        (h, rest) -> h `mappend` ":" `mappend` parseMac' rest

--------------------------------------------------------------------------------
-- | Transform a bytestring to a mac, like "\x12\xa1" to "12:a1".
parseHexMac :: BL.ByteString -> Mac
parseHexMac = parseMac . BC.concat . BL.toChunks . toLazyByteString . lazyByteStringHexFixed
