--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative  ((<|>))
import           Control.Concurrent   (threadDelay)
import           Control.Monad        (unless, when)
import           Data.List            (intercalate)
import           Data.List.Split      (splitOn)
import           Data.Time            (NominalDiffTime, UTCTime, addUTCTime,
                                       defaultTimeLocale, diffUTCTime,
                                       getCurrentTime, parseTimeM)
import qualified Network
import           System.Environment   (getArgs, getProgName)
import qualified System.IO            as IO


--------------------------------------------------------------------------------
import           CountVonCount.Config


--------------------------------------------------------------------------------
main :: IO ()
main = do
    config   <- readConfigFile "count-von-count.yaml"
    args     <- getArgs
    progName <- getProgName
    case args of
        [fileName] -> replay fileName (configSensorPort config)
        _          -> putStrLn $ "Usage: " ++ progName ++ " <replay file>"


--------------------------------------------------------------------------------
replay :: FilePath -> Int -> IO ()
replay filePath port = do
    handle <- IO.openFile filePath IO.ReadMode
    socket <- Network.connectTo "127.0.0.1" $
        Network.PortNumber $ fromIntegral port

    replayEvents Nothing handle socket

    IO.hClose handle
    IO.hClose socket


--------------------------------------------------------------------------------
replayEvents :: Maybe NominalDiffTime -> IO.Handle -> IO.Handle -> IO ()
replayEvents mDiffTime handle socket = do
    isEof <- IO.hIsEOF handle
    unless isEof $ do
        line <- IO.hGetLine handle
        case parseReplayEvent line of
            Nothing                    -> replayEvents mDiffTime handle socket
            Just (t, smac, bmac, rssi) -> do
                now        <- getCurrentTime
                mDiffTime' <- case mDiffTime of
                    Nothing -> return $ Just $ now `diffUTCTime` t
                    Just dt -> do
                        let at    = dt `addUTCTime` t
                            delay = floor $ (at `diffUTCTime` now) * 1000 * 1000
                        when (delay > 0) $ threadDelay delay
                        return mDiffTime
                IO.hPutStrLn socket $ intercalate "," [smac, "_", bmac, rssi]
                IO.hFlush socket
                replayEvents mDiffTime' handle socket


--------------------------------------------------------------------------------
parseReplayEvent :: String -> Maybe (UTCTime, String, String, String)
parseReplayEvent str = case splitOn "," str of
    [time, _, smac, _, bmac, _, rssi] ->
        case parseDateTime time of
            Just t  -> Just (t, smac, bmac, rssi)
            _ -> Nothing
    _ -> Nothing
    -- The logged datetime format has changed,
    -- so we try to parse as both formats
    where parseDateTime t = parseTimeM True defaultTimeLocale "%F %T" t <|>
                            parseTimeM True defaultTimeLocale "%T" t
