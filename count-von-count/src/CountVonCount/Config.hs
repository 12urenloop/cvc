--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Config
    ( BoxxyConfig (..)
    , defaultBoxxyConfig
    , Config (..)
    , defaultConfig
    , readConfigFile
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>), (<*>))
import           Control.Monad          (mzero)
import           Data.Aeson             (FromJSON (..), (.!=), (.:?))
import qualified Data.Aeson             as A
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BC
import           Data.Maybe             (fromMaybe, isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime (..), getCurrentTime)
import           Data.Yaml              (decodeFile)

--------------------------------------------------------------------------------
import           CountVonCount.Protocol

--------------------------------------------------------------------------------
data BoxxyConfig = BoxxyConfig
    { boxxyHost     :: Text
    , boxxyPort     :: Int
    , boxxyPath     :: Text
    , boxxyUser     :: Text
    , boxxyPassword :: Text
    }


--------------------------------------------------------------------------------
instance Show BoxxyConfig where
    show (BoxxyConfig host port path user password) =
        T.unpack host ++ ":" ++ show port ++ "/" ++ T.unpack path ++
        " (" ++ T.unpack user ++ ":" ++ T.unpack password ++ ")"


--------------------------------------------------------------------------------
instance FromJSON BoxxyConfig where
    parseJSON (A.Object o) = BoxxyConfig <$>
        o .:? "host"     .!= boxxyHost     defaultBoxxyConfig <*>
        o .:? "port"     .!= boxxyPort     defaultBoxxyConfig <*>
        o .:? "path"     .!= boxxyPath     defaultBoxxyConfig <*>
        o .:? "user"     .!= boxxyUser     defaultBoxxyConfig <*>
        o .:? "password" .!= boxxyPassword defaultBoxxyConfig

    parseJSON _ = mzero


--------------------------------------------------------------------------------
defaultBoxxyConfig :: BoxxyConfig
defaultBoxxyConfig = BoxxyConfig
    { boxxyHost     = "localhost"
    , boxxyPort     = 80
    , boxxyPath     = ""
    , boxxyUser     = "count-von-count"
    , boxxyPassword = "tetten"
    }


--------------------------------------------------------------------------------
data Config = Config
    { configStartTime             :: Maybe UTCTime
    , configCircuitLength         :: Double
    , configMaxSpeed              :: Double
    , configBatonWatchdogLifespan :: Int
    , configBatonWatchdogInterval :: Int
    , configSensorPort            :: Int
    , configLog                   :: FilePath
    , configReplayLog             :: FilePath
    , configRssiThreshold         :: Double
    , configBoxxies               :: [BoxxyConfig]
    , configWebPort               :: Int
    , configEkgPort               :: Int
    , configProtocol              :: Protocol
    } deriving (Show)


--------------------------------------------------------------------------------
instance FromJSON Config where
    parseJSON (A.Object o) = Config <$>
        o .:? "startTime"             .!= configStartTime             d <*>
        o .:? "circuitLength"         .!= configCircuitLength         d <*>
        o .:? "maxSpeed"              .!= configMaxSpeed              d <*>
        o .:? "batonWatchdogLifespan" .!= configBatonWatchdogLifespan d <*>
        o .:? "batonWatchdogInterval" .!= configBatonWatchdogInterval d <*>
        o .:? "sensorPort"            .!= configSensorPort            d <*>
        o .:? "log"                   .!= configLog                   d <*>
        o .:? "replayLog"             .!= configReplayLog             d <*>
        o .:? "rssiThreshold"         .!= configRssiThreshold         d <*>
        o .:? "boxxies"               .!= configBoxxies               d <*>
        o .:? "webPort"               .!= configWebPort               d <*>
        o .:? "ekgPort"               .!= configEkgPort               d <*>
        o .:? "protocol"              .!= configProtocol              d
      where
        d = defaultConfig

    parseJSON _ = mzero


--------------------------------------------------------------------------------
defaultConfig :: Config
defaultConfig = Config
    { configStartTime             = Nothing
    , configCircuitLength         = 400
    , configMaxSpeed              = 12  -- 12m/s should be plenty?
    , configBatonWatchdogLifespan = 30
    , configBatonWatchdogInterval = 5
    , configSensorPort            = 9001
    , configLog                   = "log/count-von-count.log"
    , configReplayLog             = "log/replay.log"
    , configRssiThreshold         = -81
    , configBoxxies               = [defaultBoxxyConfig]
    , configWebPort               = 8000
    , configEkgPort               = 8001
    , configProtocol              = csv
    }


--------------------------------------------------------------------------------
readConfigFile :: FilePath -> IO Config
readConfigFile filePath = do
    config <- fromMaybe (error $ "Could not read config: " ++ filePath) <$>
        decodeFile filePath
    if isNothing $ configStartTime config
        then getCurrentTime >>= \t -> return config { configStartTime = Just t }
        else return config
