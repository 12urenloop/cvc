--------------------------------------------------------------------------------
-- | Communication with boxxy
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CountVonCount.Boxxy
    ( -- * Configuration
      BoxxyConfig (..)
    , defaultBoxxyConfig

      -- * Talking to boxxy
    , putState
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
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BC
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
    { boxxyHost     :: Text
    , boxxyPort     :: Int
    , boxxyPath     :: Text
    , boxxyUser     :: ByteString
    , boxxyPassword :: ByteString
    }


--------------------------------------------------------------------------------
instance Show BoxxyConfig where
    show (BoxxyConfig host port path user password) =
        T.unpack host ++ ":" ++ show port ++ "/" ++ T.unpack path ++
        " (" ++ BC.unpack user ++ ":" ++ BC.unpack password ++ ")"


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
makeRequest :: ToJSON a => BoxxyConfig -> Text -> a -> IO ()
makeRequest config path body = do
    let rq = Http.applyBasicAuth (boxxyUser config) (boxxyPassword config) $
                Http.def
                    { Http.method         = "PUT"
                    , Http.host           = T.encodeUtf8 (boxxyHost config)
                    , Http.port           = boxxyPort config
                    , Http.path           = T.encodeUtf8 path'
                    , Http.requestBody    = Http.RequestBodyLBS (A.encode body)
                    , Http.requestHeaders =
                        [ ("Connection", "Close")
                        , ("Content-Type", "application/json")
                        ]
                    }

    manager <- Http.newManager Http.def
    _       <- C.runResourceT $ Http.httpLbs rq manager
    Http.closeManager manager
  where
    path' = boxxyPath config `T.append` path


--------------------------------------------------------------------------------
putState :: BoxxyConfig -> [Team] -> [Lap] -> IO ()
putState config teams laps = makeRequest config "/state" $ A.object
    [ "teams" .= teams
    , "laps"  .= laps
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
