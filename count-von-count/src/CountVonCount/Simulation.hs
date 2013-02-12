--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative       ((<$>))
import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forM, forM_, forever)
import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.State       (StateT, evalStateT, get, modify)
import           Control.Monad.Trans       (liftIO)
import           Data.Maybe                (listToMaybe)
import qualified Data.Text                 as T
import qualified System.Console.ANSI       as Ansi
import           System.Random             (randomRIO)


--------------------------------------------------------------------------------
import           CountVonCount.Config
import           CountVonCount.Management
import           CountVonCount.Persistence


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
            { simulationTeams = [(t, b, 0) | (t, Just b) <- teams]
            }

    evalStateT (runReaderT simulation simulationRead) simulationState


--------------------------------------------------------------------------------
type Position = Int


--------------------------------------------------------------------------------
data SimulationRead = SimulationRead
    { simulationStations :: [Station]
    , simulationConfig   :: Config
    , simulationLength   :: Position
    }


--------------------------------------------------------------------------------
data SimulationState = SimulationState
    { simulationTeams :: [(Team, Baton, Position)]
    }


--------------------------------------------------------------------------------
type Simulation = ReaderT SimulationRead (StateT SimulationState IO)


--------------------------------------------------------------------------------
simulation :: Simulation ()
simulation = do
    liftIO $ Ansi.clearScreen
    forever $ do
        render
        step
        liftIO $ threadDelay $ 100 * 1000


--------------------------------------------------------------------------------
stationAt :: Position -> Simulation (Maybe Station)
stationAt pos = do
    stations <- simulationStations <$> ask
    len      <- simulationLength   <$> ask
    config   <- simulationConfig   <$> ask
    let step' = configCircuitLength config / fromIntegral len
    return $ listToMaybe $
        [s | s <- stations, pos == floor (stationPosition s / step')]


--------------------------------------------------------------------------------
render :: Simulation ()
render = do
    teams <- simulationTeams    <$> get
    len   <- simulationLength   <$> ask
    let nameLen = maximum [T.length (teamName t) | (t, _, _) <- teams] + 1

    liftIO $ Ansi.setCursorPosition 0 0

    stationLine <- forM [0 .. len - 1] $ \p -> do
        station <- stationAt p
        return $ maybe ' ' (const 'S') station
    liftIO $ putStrLn $ replicate nameLen ' ' ++ stationLine

    forM_ teams $ \(t, _, p) -> liftIO $ putStrLn $
        pad nameLen (T.unpack (teamName t)) ++
        [if i == p then 'X' else ' ' | i <- [0 .. len - 1]]
  where
    pad len str = str ++ replicate (len - length str) ' '


--------------------------------------------------------------------------------
step :: Simulation ()
step = do
    teams <- simulationTeams <$> get
    len   <- simulationLength <$> ask
    idx   <- liftIO $ randomRIO (0, length teams - 1)

    let teams' =
            [ if i == idx then (t, b, (p + 1) `mod` len) else (t, b, p)
            | (i, (t, b, p)) <- zip [0 ..] teams
            ]

    modify $ \s -> s {simulationTeams = teams'}
