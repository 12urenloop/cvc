--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module  CountVonCount.Protocol
    ( Protocol (..)
    , csv
    , gyrid
    ) where

--------------------------------------------------------------------------------
import           Control.Monad                 (mzero)
import           Data.Aeson
import qualified Data.ByteString               as B
import           System.IO.Streams

--------------------------------------------------------------------------------
import           CountVonCount.Log
import           CountVonCount.Protocols.CSV   (csvInput, csvOutput)
import           CountVonCount.Protocols.Gyrid (gyridInput, gyridOutput)
import           CountVonCount.RawSensorEvent

--------------------------------------------------------------------------------
data Protocol = Protocol
    { name   :: String
    , output :: OutputStream B.ByteString -> IO ()
    , input  :: Log -> InputStream B.ByteString
                    -> IO (InputStream RawSensorEvent)
    }

instance Show Protocol where
    show = name

instance FromJSON Protocol where
    parseJSON (String s) = return $ case s of
        "gyrid" -> gyrid
        _       -> csv
    parseJSON _ = mzero

--------------------------------------------------------------------------------
csv :: Protocol
csv = Protocol
    { name = "CSV"
    , input  = csvInput
    , output = csvOutput
    }

--------------------------------------------------------------------------------
gyrid :: Protocol
gyrid = Protocol
    { name = "Gyrid"
    , input  = gyridInput
    , output = gyridOutput
    }

