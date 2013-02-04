--------------------------------------------------------------------------------
-- | Communication with boxxy
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CountVonCount.Boxxy
    ( -- * Configuration
      BoxxyConfig (..)
    , defaultBoxxyConfig

      -- * Talking to boxxy
    , putConfig
    , putLaps
    , putPosition

      -- * Stateful talking
    , BoxxyState (..)
    , Boxxies
    , newBoxxies
    , withBoxxies
    , boxxiesToList
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative        (pure, (<$>), (<*>))
import           Control.Concurrent         (forkIO)
import           Control.Monad              (forM, forM_, mzero, void, when)
import           Data.Aeson                 (FromJSON (..), ToJSON (..), (.!=),
                                             (.:?), (.=))
import qualified Data.Aeson                 as A
import qualified Data.Conduit               as C
import           Data.IORef                 (IORef, newIORef, readIORef,
                                             writeIORef)
import           Data.Maybe                 (isNothing)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Time                  (UTCTime)
import qualified Network.HTTP.Conduit       as Http


--------------------------------------------------------------------------------
import           CountVonCount.Counter.Core
import           CountVonCount.EventBase
import           CountVonCount.Log          (Log)
import qualified CountVonCount.Log          as Log
import           CountVonCount.Persistence
import           CountVonCount.Util


--------------------------------------------------------------------------------
data BoxxyConfig = BoxxyConfig
    { boxxyHost :: Text
    , boxxyPort :: Int
    , boxxyPath :: Text
    , boxxyKey  :: Text
    }


--------------------------------------------------------------------------------
instance Show BoxxyConfig where
    show (BoxxyConfig host port path key) = T.unpack host ++ ":" ++
        show port ++ "/" ++ T.unpack path ++ " (" ++ show key ++ ")"


--------------------------------------------------------------------------------
instance ToJSON BoxxyConfig where
    toJSON conf = A.object
        [ "host" .= boxxyHost conf
        , "port" .= boxxyPort conf
        , "path" .= boxxyPath conf
        , "key"  .= boxxyKey  conf
        ]


--------------------------------------------------------------------------------
instance FromJSON BoxxyConfig where
    parseJSON (A.Object o) = BoxxyConfig <$>
        o .:? "host" .!= boxxyHost defaultBoxxyConfig <*>
        o .:? "port" .!= boxxyPort defaultBoxxyConfig <*>
        o .:? "path" .!= boxxyPath defaultBoxxyConfig <*>
        o .:? "key"  .!= boxxyKey  defaultBoxxyConfig

    parseJSON _ = mzero


--------------------------------------------------------------------------------
defaultBoxxyConfig :: BoxxyConfig
defaultBoxxyConfig = BoxxyConfig
    { boxxyHost = "localhost"
    , boxxyPort = 80
    , boxxyPath = ""
    , boxxyKey  = "tetten"
    }


--------------------------------------------------------------------------------
makeRequest :: ToJSON a => BoxxyConfig -> Text -> a -> IO ()
makeRequest config path body = do
    let rq = Http.def
            { Http.method         = "PUT"
            , Http.host           = T.encodeUtf8 (boxxyHost config)
            , Http.port           = boxxyPort config
            , Http.path           = T.encodeUtf8 path'
            , Http.requestBody    = Http.RequestBodyLBS (A.encode body)
            , Http.queryString    = T.encodeUtf8 queryString
            , Http.requestHeaders =
                [ ("Connection", "Close")
                , ("Content-Type", "application/json")
                ]
            }

    manager <- Http.newManager Http.def
    _       <- C.runResourceT $ Http.httpLbs rq manager
    Http.closeManager manager
  where
    path'       = boxxyPath config `T.append` path
    queryString = "key=" `T.append` boxxyKey config


--------------------------------------------------------------------------------
putConfig :: BoxxyConfig -> UTCTime -> Double -> [Station] -> [Team] -> UTCTime
          -> IO ()
putConfig config startTime circuitLength stations teams time =
    makeRequest config "/config" $ A.object
        [ "startTime"     .= startTime
        , "circuitLength" .= circuitLength
        , "stations"      .= stations
        , "teams"         .= teams
        , "time"          .= time
        ]


--------------------------------------------------------------------------------
putLaps :: BoxxyConfig   -- ^ Boxxy instance to notify
        -> Team          -- ^ Applicable team
        -> UTCTime       -- ^ Time of event
        -> Int           -- ^ Number of points added
        -> Maybe Double  -- ^ Average lap speed
        -> Maybe Text    -- ^ Lap reason (for bonus rounds)
        -> IO ()
putLaps config team time count speed reason = makeRequest config path $ A.object
    [ "team"   .= team
    , "time"   .= time
    , "count"  .= count
    , "speed"  .= speed
    , "reason" .= reason
    ]
  where
    path = T.concat ["/", refToText (teamId team), "/laps"]


--------------------------------------------------------------------------------
putPosition :: BoxxyConfig -> Team -> UTCTime -> Station -> Double -> IO ()
putPosition config team time station speed = makeRequest config path $ A.object
    [ "team"    .= team
    , "station" .= station
    , "speed"   .= speed
    , "time"    .= time
    ]
  where
    path = T.concat ["/", refToText (teamId team), "/position"]


--------------------------------------------------------------------------------
data BoxxyState = Up | Down
    deriving (Eq, Show)


--------------------------------------------------------------------------------
data Boxxies = Boxxies
    { boxxiesState :: [(BoxxyConfig, IORef BoxxyState)]
    , boxxiesInit  :: BoxxyConfig -> IO ()
    }


--------------------------------------------------------------------------------
newBoxxies :: Log -> EventBase -> [BoxxyConfig] -> (BoxxyConfig -> IO ())
           -> IO Boxxies
newBoxxies logger eventBase configs ini = do
    bs <- Boxxies <$> forM configs (\c -> (,) c <$> newIORef Down) <*> pure ini
    withBoxxies logger bs (const $ return ())

    -- Subscribe to counter events
    subscribe eventBase "boxxies counter handler" $
        \(team, _ :: CounterState, event) ->
            withBoxxies logger bs $ \b -> case event of
                LapEvent time speed                 ->
                    putLaps b team time 1 (Just speed) Nothing
                ProgressionEvent time station speed ->
                    putPosition b team time station speed

    return bs


--------------------------------------------------------------------------------
withBoxxies :: Log
            -> Boxxies
            -> (BoxxyConfig -> IO ())
            -> IO ()
withBoxxies logger bs f = forM_ (boxxiesState bs) $ \(c, rs) ->
    void $ forkIO $ do
        s <- readIORef rs
        Log.string logger "CountVonCount.Boxxy.withBoxxies" $
            "Calling " ++ show c ++ ", currently " ++ show s
        -- Try to init if needed
        r <- case s of
            Down -> isolate logger ("init " ++ show c) $ boxxiesInit bs c
            Up   -> return Nothing

        -- Make the call if up
        r' <- case r of
            Nothing -> isolate logger ("call " ++ show c) $ f c
            Just _  -> return r

        let s' = if isNothing r' then Up else Down
        when (s /= s') $ Log.string logger "CountVonCount.Boxxy.withBoxxies" $
            show c ++ " is now " ++ show s'
        writeIORef rs s'


--------------------------------------------------------------------------------
boxxiesToList :: Boxxies -> IO [(BoxxyConfig, BoxxyState)]
boxxiesToList boxxies = forM (boxxiesState boxxies) $ \(c, rs) -> do
    s <- readIORef rs
    return (c, s)
