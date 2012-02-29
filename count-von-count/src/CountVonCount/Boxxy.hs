-- | Communication with boxxy
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Boxxy
    ( -- * Configuration
      BoxxyConfig (..)
    , defaultBoxxyConfig

      -- * Talking to boxxy
    , putInitialization
    , putLaps
    , putPosition
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Data.Maybe (fromMaybe)

import Data.Aeson (FromJSON (..), ToJSON (..), (.=), (.:?), (.!=))
import Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Conduit as Http

import CountVonCount.Persistence
import CountVonCount.Types

data BoxxyConfig = BoxxyConfig
    { boxxyHost :: Text
    , boxxyPort :: Int
    , boxxyPath :: Text
    , boxxyKey  :: Text
    } deriving (Show)

instance ToJSON BoxxyConfig where
    toJSON conf = A.object
        [ "host" .= boxxyHost conf
        , "port" .= boxxyPort conf
        , "path" .= boxxyPath conf
        , "key"  .= boxxyKey  conf
        ]

instance FromJSON BoxxyConfig where
    parseJSON (A.Object o) = BoxxyConfig <$>
        o .:? "host" .!= boxxyHost defaultBoxxyConfig <*>
        o .:? "port" .!= boxxyPort defaultBoxxyConfig <*>
        o .:? "path" .!= boxxyPath defaultBoxxyConfig <*>
        o .:? "key"  .!= boxxyKey  defaultBoxxyConfig

    parseJSON _ = mzero

defaultBoxxyConfig :: BoxxyConfig
defaultBoxxyConfig = BoxxyConfig
    { boxxyHost = "localhost"
    , boxxyPort = 80
    , boxxyPath = ""
    , boxxyKey  = "tetten"
    }

makeRequest :: ToJSON a => BoxxyConfig -> Text -> a -> IO ()
makeRequest config path body = do
    let rq = Http.def
            { Http.method         = "PUT"
            , Http.host           = T.encodeUtf8 (boxxyHost config)
            , Http.port           = boxxyPort config
            , Http.path           = T.encodeUtf8 path'
            , Http.requestBody    = Http.RequestBodyLBS (A.encode body)
            , Http.requestHeaders = [("Connection", "Close")]
            }

    manager <- Http.newManager Http.def
    _       <- C.runResourceT $ Http.httpLbs rq manager
    Http.closeManager manager
  where
    path' = boxxyPath config `T.append` path

putInitialization :: BoxxyConfig -> [Team] -> IO ()
putInitialization config teams = makeRequest config "/teams" teams

putLaps :: BoxxyConfig -> Team -> Maybe Text -> Maybe Int -> IO ()
putLaps config team reason count = makeRequest config path $ A.object
    [ "team"   .= team
    , "reason" .= reason
    , "count"  .= fromMaybe 1 count
    ]
  where
    path = T.concat ["/", T.pack (show $ teamId team), "/laps"]

putPosition :: BoxxyConfig -> Team -> Station -> Double -> IO ()
putPosition config team station speed = makeRequest config path $ A.object
    [ "station" .= station
    , "speed"   .= speed
    ]
  where
    path = T.concat ["/", T.pack (show $ teamId team), "/position"]
