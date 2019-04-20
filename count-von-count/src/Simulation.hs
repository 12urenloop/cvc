--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Concurrent        (threadDelay)
import           Control.Exception         (IOException, handle)
import           Control.Monad             (forM, forM_, forever)
import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.State       (StateT, evalStateT, get, modify)
import           Control.Monad.Trans       (liftIO)
import           Data.List                 (intercalate)
import           Data.Maybe                (catMaybes, listToMaybe)
import qualified Data.Text                 as T
import qualified Network.Socket            as S
import qualified System.Console.ANSI       as Ansi
import           System.IO                 as IO
import           System.Random             (randomRIO)


--------------------------------------------------------------------------------
import           CountVonCount.Config
import           CountVonCount.Management
import           CountVonCount.Persistence

--------------------------------------------------------------------------------
detectionChance :: Double
detectionChance = 0.1

--------------------------------------------------------------------------------
main :: IO ()
main = do
    config     <- readConfigFile "count-von-count.yaml"
    database   <- newDatabase "count-von-count.db"
    (teams, _) <- assignment database
    stations   <- getAllStations database
    closeDatabase database

    let simulationRead = SimulationRead
            { simulationStations = stations
            , simulationConfig   = config
            , simulationLength   = 20
            }

        simulationState = SimulationState
            { simulationTeams  = [(t, b, 0, 0, -1) | (t, Just b) <- teams]
            , simulationSocket = Nothing
            }

    evalStateT (runReaderT simulation simulationRead) simulationState


--------------------------------------------------------------------------------
type Position = Int


--------------------------------------------------------------------------------
type LastDetected = Int


--------------------------------------------------------------------------------
type Rounds = Int


--------------------------------------------------------------------------------
data SimulationRead = SimulationRead
    { simulationStations :: [Station]
    , simulationConfig   :: Config
    , simulationLength   :: Position
    }


--------------------------------------------------------------------------------
data SimulationState = SimulationState
    { simulationTeams  :: [(Team, Baton, Position, Rounds, LastDetected)]
    , simulationSocket :: Maybe IO.Handle
    }


--------------------------------------------------------------------------------
type Simulation = ReaderT SimulationRead (StateT SimulationState IO)


--------------------------------------------------------------------------------
simulation :: Simulation ()
simulation = do
    liftIO Ansi.clearScreen
    forever $ do
        render
        step
        sensor
        liftIO $ threadDelay $ 1000 * 200


--------------------------------------------------------------------------------
stationAt :: Position -> Simulation (Maybe Station)
stationAt pos = do
    stations <- simulationStations <$> ask
    len      <- simulationLength   <$> ask
    config   <- simulationConfig   <$> ask
    let step' = configCircuitLength config / fromIntegral len
    return $ listToMaybe
        [s | s <- stations, pos == floor (stationPosition s / step')]


--------------------------------------------------------------------------------
render :: Simulation ()
render = do
    teams <- simulationTeams    <$> get
    len   <- simulationLength   <$> ask
    let nameLen = maximum [T.length (teamName t) | (t, _, _, _, _) <- teams]

    -- Clear the page
    liftIO $ Ansi.setCursorPosition 0 0

    -- Top line, shows the position of the stations
    -- and the current detection chance
    stationLine <- forM [0 .. len - 1] $ \p -> do
        station <- stationAt p
        return $ maybe ' ' (const 'S') station
    liftIO $ putStrLn $ replicate (nameLen + 2) ' '
                        ++ stationLine

    -- Line for each team
    forM_ teams $ \(team, _, pos, rnd, lst) -> liftIO $ putStrLn $
        pad nameLen (T.unpack (teamName team))   -- Team name (padded)
        ++ " |"
        ++ progress pos lst len                  -- Team progress
        ++ "| "
        ++ show rnd                              -- Team rounds

    -- Legend
    liftIO $ putStrLn $
        replicate nameLen ' '
        ++ "  X = current position\n"
        ++ replicate nameLen ' '
        ++ "  ? = last detected\n"
        ++ replicate nameLen ' '
        ++ "  Detection chance: "
        ++ show (detectionChance * 100) ++ "%"

  where
    pad len str       = str ++ replicate (len - length str) ' '
    progress p l len  = [ positionChar i p l | i <- [0 .. len - 1]]
    positionChar i p l
      | i == p    = 'X'
      | i == l    = '?'
      | otherwise = ' '


--------------------------------------------------------------------------------
step :: Simulation ()
step = do
    teams <- simulationTeams <$> get
    len   <- simulationLength <$> ask
    idx   <- liftIO $ randomRIO (0, length teams - 1)

    let teams' =
            [ if i == idx
                then (t, b, (p + 1) `mod` len, r + ((p + 1) `quot` len ), l)
                else (t, b, p, r, l)
            | (i, (t, b, p, r, l)) <- zip [0 ..] teams
            ]

    modify $ \s -> s {simulationTeams = teams'}


--------------------------------------------------------------------------------
sensor :: Simulation ()
sensor = do
    teams   <- simulationTeams  <$> get
    msocket <- simulationSocket <$> get
    config  <- simulationConfig <$> ask

    -- If a team passes a station, it has a chance to be detected
    sensed <- forM teams $ \(t, b, p, r, l) -> do
        ms <- stationAt p
        rand <- liftIO $ randomRIO (0.0, 1.0)
        case ms of
            Nothing -> return ((t, b, p, r, l), Nothing)
            Just s  -> return $ if rand < detectionChance
                                then ((t, b, p, r, p), Just s)
                                else ((t, b, p, r, l), Nothing)

    -- Update teams with 'LastDetected'
    modify $ \s -> s {simulationTeams = fmap fst sensed}

    -- Send detections to CVC over the socket
    -- (try to connect a new socket if it was disconected)
    socket <- liftIO $ handle handler $ do
        socket <- case msocket of
            Just s  -> return s
            Nothing -> connectTo $ configSensorPort config

        let detections = catMaybes $ fmap detect sensed
        forM_ detections $
           \(sMac, bMac) -> IO.hPutStrLn socket $
              intercalate "," [T.unpack sMac, "_", T.unpack bMac, "0"]

        IO.hFlush socket
        return $ Just socket

    -- Update the socket
    modify $ \s -> s {simulationSocket = socket}
  where
    detect ((_, b, _, _, _), Just s ) = Just (stationMac s, batonMac b)
    detect ( _,              Nothing) = Nothing

    handler :: IOException -> IO (Maybe IO.Handle)
    handler _ = return Nothing
    connectTo port = S.withSocketsDo $ do -- Networking boilerplate
      let hints = S.defaultHints { S.addrSocketType = S.Stream }
      addr:_ <- S.getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show port)
      socket <- S.socket (S.addrFamily addr)
                         (S.addrSocketType addr)
                         (S.addrProtocol addr)
      S.connect socket $ S.addrAddress addr
      S.socketToHandle socket IO.WriteMode
