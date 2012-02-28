-- | Communication with boxxy
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Boxxy
    ( BoxxyConfig (..)
    , defaultBoxxyConfig
    , initialize
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)

import Data.Aeson (FromJSON (..), ToJSON (..), (.=), (.:?), (.!=))
import Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Conduit as Http

import CountVonCount.Persistence

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
            { Http.method      = "PUT"
            , Http.host        = T.encodeUtf8 (boxxyHost config)
            , Http.port        = boxxyPort config
            , Http.path        = T.encodeUtf8 (boxxyPath config `T.append` path)
            , Http.requestBody = Http.RequestBodyLBS (A.encode body)
            }

    manager <- Http.newManager Http.def
    _       <- C.runResourceT $ Http.httpLbs rq manager
    Http.closeManager manager

initialize :: BoxxyConfig -> [Team] -> IO ()
initialize config teams = makeRequest config "/teams" teams
