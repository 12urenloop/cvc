{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Types
    ( Mac
    , Station (..)
    , Baton (..)
    , batonName
    , Handler
    , handler
    , callHandler
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero)
import Data.Ord (comparing)
import Text.Printf (printf)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.Text as T

import CountVonCount.Util
import CountVonCount.Log

type Mac = Text

data Station = Station
    { stationName     :: String
    , stationMac      :: Mac
    , stationPosition :: Double
    }

instance Show Station where
    show s = printf "%s (%.0fm)" (stationName s) (stationPosition s)

instance Eq Station where
    s1 == s2 = stationMac s1 == stationMac s2

instance Ord Station where
    compare = comparing stationPosition

instance ToJSON Station where
    toJSON (Station name mac position) = A.object
        ["name" .= name, "mac" .= mac, "position" .= position]

instance FromJSON Station where
    parseJSON (A.Object o) = Station <$>
        o .: "name" <*> o .: "mac" <*> o .: "position"
    parseJSON _ = mzero

data Baton = Baton
    { batonMac  :: Mac
    , batonNr   :: Int
    } deriving (Eq)

instance Show Baton where
    show b = batonName b ++ " (" ++ T.unpack (batonMac b) ++ ")"

instance Ord Baton where
    compare = comparing batonNr

instance ToJSON Baton where
    toJSON (Baton mac nr) = A.object ["mac" .= mac, "nr" .= nr]

instance FromJSON Baton where
    parseJSON (A.Object o) = Baton <$> o .: "mac" <*> o .: "nr"
    parseJSON _ = mzero

batonName :: Baton -> String
batonName = ("Baton " ++) . show . batonNr

data Handler a = Handler (Log -> a -> IO ())

handler :: String -> (a -> IO ()) -> Handler a
handler name f = Handler $ \logger -> isolate logger ("Handler " ++ name) . f

callHandler :: Log -> Handler a -> a -> IO ()
callHandler logger (Handler f) = f logger
