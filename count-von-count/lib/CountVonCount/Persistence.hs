--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CountVonCount.Persistence
    ( Database
    , newDatabase
    , closeDatabase

    , Ref
    , refToString
    , refFromString
    , refToText

    , Team (..)
    , addTeam
    , getTeam
    , getTeamByMac
    , getAllTeams
    , setTeamBaton

    , Lap (..)
    , addLap
    , addLaps
    , getLap
    , getLatestLaps

    , Station (..)
    , addStation
    , getAllStations
    , getStationByMac

    , Baton (..)
    , addBaton
    , getBaton
    , getAllBatons
    , getBatonByMac

    , deleteAll
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative     ((<$>), (<*>))
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Exception       (finally)
import           Data.Int                (Int64)
import           Data.Maybe              (listToMaybe)
import           Data.Ord                (comparing)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time               (UTCTime)
import           Data.Typeable           (Typeable)
import           Database.SQLite.Simple  (FromRow (..))
import qualified Database.SQLite.Simple  as Sqlite
import           Text.Printf             (printf)
import           Prelude


--------------------------------------------------------------------------------
import           CountVonCount.Types


--------------------------------------------------------------------------------
data Database = Database
    { databaseConnection :: Sqlite.Connection
    , databaseLock       :: MVar ()
    }


--------------------------------------------------------------------------------
newDatabase :: FilePath -> IO Database
newDatabase fp = do
    c <- Sqlite.open fp
    mapM_ (Sqlite.execute_ c) $ concat
        [teamsTable, lapsTable, stationsTable, batonsTable]
    Database c <$> newMVar ()


--------------------------------------------------------------------------------
closeDatabase :: Database -> IO ()
closeDatabase (Database c _) = Sqlite.close c


--------------------------------------------------------------------------------
withConnection :: Database -> (Sqlite.Connection -> IO a) -> IO a
withConnection db f = do
    () <- takeMVar $ databaseLock db
    finally (f $ databaseConnection db) $ putMVar (databaseLock db) ()


--------------------------------------------------------------------------------
type Ref a = Int64


--------------------------------------------------------------------------------
refToString :: Ref a -> String
refToString = show


--------------------------------------------------------------------------------
refFromString :: String -> Ref a
refFromString = read


--------------------------------------------------------------------------------
refToText :: Ref a -> Text
refToText = T.pack . refToString


--------------------------------------------------------------------------------
data Team = Team
    { teamId    :: Ref Team
    , teamName  :: Text
    , teamLaps  :: Int
    , teamBaton :: Maybe (Ref Baton)
    } deriving (Typeable)


--------------------------------------------------------------------------------
instance Eq Team where
    t1 == t2 = teamId t1 == teamId t2


--------------------------------------------------------------------------------
instance Ord Team where
    compare = comparing teamId


--------------------------------------------------------------------------------
instance Show Team where
    show = T.unpack . teamName


--------------------------------------------------------------------------------
instance FromRow Team where
    fromRow = Team
        <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field <*> Sqlite.field


--------------------------------------------------------------------------------
teamsTable :: [Sqlite.Query]
teamsTable =
    [ "CREATE TABLE IF NOT EXISTS teams ( \
      \    id INTEGER PRIMARY KEY,        \
      \    name TEXT,                     \
      \    laps INTEGER,                  \
      \    baton_id INTEGER               \
      \)"
    , "CREATE INDEX IF NOT EXISTS teams_baton_id ON teams (baton_id)"
    ]


--------------------------------------------------------------------------------
addTeam :: Database -> Text -> IO (Ref Team)
addTeam db name = withConnection db $ \c -> do
    Sqlite.execute c "INSERT INTO teams (name, laps, baton_id) VALUES (?, ?, ?)"
        (name, 0 :: Int, Nothing :: Maybe Int)
    Sqlite.lastInsertRowId c


--------------------------------------------------------------------------------
getTeam :: Database -> Ref Team -> IO Team
getTeam db ref = withConnection db $ \c -> do
    teams <- Sqlite.query c "SELECT * FROM teams WHERE id = ?" (Sqlite.Only ref)
    case teams of
        (x : _) -> return x
        []      -> error $ "No team with ID " ++ refToString ref


--------------------------------------------------------------------------------
getTeamByMac :: Database -> Mac -> IO (Maybe Team)
getTeamByMac db mac = withConnection db $ \c -> listToMaybe <$> Sqlite.query c
    "SELECT teams.* FROM teams, batons \
    \WHERE teams.baton_id = batons.id AND mac = ?"
    (Sqlite.Only mac)


--------------------------------------------------------------------------------
getAllTeams :: Database -> IO [Team]
getAllTeams db = withConnection db $ \c -> Sqlite.query_ c "SELECT * FROM teams"


--------------------------------------------------------------------------------
setTeamBaton :: Database -> Ref Team -> Maybe (Ref Baton) -> IO ()
setTeamBaton db ref baton = withConnection db $ \c -> Sqlite.execute c
    "UPDATE teams SET baton_id = ? WHERE id = ?" (baton, ref)


--------------------------------------------------------------------------------
data Lap = Lap
    { lapId        :: Ref Lap
    , lapTeam      :: Ref Team
    , lapTimestamp :: UTCTime
    , lapReason    :: Maybe Text
    , lapCount     :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
instance FromRow Lap where
    fromRow = Lap
        <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field
        <*> Sqlite.field <*> Sqlite.field


--------------------------------------------------------------------------------
lapsTable :: [Sqlite.Query]
lapsTable = return
    "CREATE TABLE IF NOT EXISTS laps (             \
    \    id INTEGER PRIMARY KEY,                   \
    \    team_id INTEGER,                          \
    \    timestamp TIMESTAMP,                      \
    \    reason TEXT,                              \
    \    count INTEGER,                            \
    \    FOREIGN KEY(team_id) REFERENCES team (id) \
    \)"


--------------------------------------------------------------------------------
addLap :: Database -> Ref Team -> UTCTime -> IO (Ref Lap)
addLap db ref timestamp = addLaps db ref timestamp Nothing 1


--------------------------------------------------------------------------------
addLaps :: Database -> Ref Team -> UTCTime -> Maybe Text -> Int -> IO (Ref Lap)
addLaps db !ref !timestamp !reason !count = withConnection db $ \c -> do
    Sqlite.execute c
        "INSERT INTO laps (team_id, timestamp, reason, count) \
        \VALUES (?, ?, ?, ?)"
        (ref, timestamp, reason, count)
    lapId' <- Sqlite.lastInsertRowId c
    Sqlite.execute c
        "UPDATE teams SET laps = laps + ? WHERE id = ?"
        (count, ref)
    return lapId'


--------------------------------------------------------------------------------
getLap :: Database -> Ref Lap -> IO Lap
getLap db ref = withConnection db $ \c -> do
    laps <- Sqlite.query c "SELECT * FROM laps WHERE id = ?" (Sqlite.Only ref)
    case laps of
        (x : _) -> return x
        []      -> error $ "No lap with ID " ++ refToString ref


--------------------------------------------------------------------------------
getLatestLaps :: Database -> Maybe (Ref Team) -> Int -> IO [Lap]
getLatestLaps db Nothing     n = withConnection db $ \c -> Sqlite.query c
    "SELECT * FROM laps ORDER BY id DESC LIMIT ?" (Sqlite.Only n)
getLatestLaps db !(Just ref) n = withConnection db $ \c -> Sqlite.query c
    "SELECT * FROM laps WHERE team_id = ? ORDER BY id DESC LIMIT ?"
    (ref, n)


--------------------------------------------------------------------------------
data Station = Station
    { stationId       :: Ref Station
    , stationName     :: Text
    , stationMac      :: Mac
    , stationPosition :: Double
    }


--------------------------------------------------------------------------------
instance Show Station where
    show s = printf "%s (%.0fm)" (T.unpack $ stationName s) (stationPosition s)


--------------------------------------------------------------------------------
instance Eq Station where
    s1 == s2 = stationMac s1 == stationMac s2


--------------------------------------------------------------------------------
instance Ord Station where
    compare = comparing stationPosition


--------------------------------------------------------------------------------
instance FromRow Station where
    fromRow = Station
        <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field <*> Sqlite.field


--------------------------------------------------------------------------------
stationsTable :: [Sqlite.Query]
stationsTable =
    [ "CREATE TABLE IF NOT EXISTS stations ( \
      \    id INTEGER PRIMARY KEY,           \
      \    name TEXT,                        \
      \    mac TEXT,                         \
      \    position REAL                     \
      \)"
    , "CREATE INDEX IF NOT EXISTS stations_mac ON stations (mac)"
    ]


--------------------------------------------------------------------------------
addStation :: Database -> Text -> Mac -> Double -> IO ()
addStation db name mac position = withConnection db $ \c -> Sqlite.execute c
    "INSERT INTO stations (name, mac, position) VALUES (?, ?, ?)"
    (name, mac, position)


--------------------------------------------------------------------------------
getAllStations :: Database -> IO [Station]
getAllStations db = withConnection db $ \c ->
    Sqlite.query_ c "SELECT * FROM stations"


--------------------------------------------------------------------------------
getStationByMac :: Database -> Mac -> IO (Maybe Station)
getStationByMac db mac = withConnection db $ \c -> listToMaybe <$>
    Sqlite.query c "SELECT * FROM stations WHERE mac = ?" (Sqlite.Only mac)


--------------------------------------------------------------------------------
data Baton = Baton
    { batonId   :: Ref Baton
    , batonMac  :: Mac
    , batonName :: Text
    } deriving (Eq)


--------------------------------------------------------------------------------
instance Show Baton where
    show b = T.unpack (batonName b) ++ " (" ++ T.unpack (batonMac b) ++ ")"


--------------------------------------------------------------------------------
instance Ord Baton where
    compare = comparing batonName


--------------------------------------------------------------------------------
instance FromRow Baton where
    fromRow = Baton <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field


--------------------------------------------------------------------------------
batonsTable :: [Sqlite.Query]
batonsTable =
    [ "CREATE TABLE IF NOT EXISTS batons ( \
      \    id INTEGER PRIMARY KEY,         \
      \    mac TEXT,                       \
      \    name TEXT                       \
      \)"
    , "CREATE INDEX IF NOT EXISTS batons_mac ON batons (mac)"
    ]


--------------------------------------------------------------------------------
addBaton :: Database -> Mac -> Text -> IO (Ref Baton)
addBaton db mac nr = withConnection db $ \c -> do
    Sqlite.execute c "INSERT INTO batons (mac, name) VALUES (?, ?)" (mac, nr)
    Sqlite.lastInsertRowId c


--------------------------------------------------------------------------------
getBaton :: Database -> Ref Baton -> IO Baton
getBaton db id' = withConnection db $ \c -> do
    bs <- Sqlite.query c "SELECT * FROM batons WHERE id = ?" (Sqlite.Only id')
    case bs of
        (x : _) -> return x
        _       -> error $ "No baton with ref " ++ show id'


--------------------------------------------------------------------------------
getAllBatons :: Database -> IO [Baton]
getAllBatons db = withConnection db $ \c ->
    Sqlite.query_ c "SELECT * FROM batons"


--------------------------------------------------------------------------------
getBatonByMac :: Database -> Mac -> IO (Maybe Baton)
getBatonByMac db mac = withConnection db $ \c -> listToMaybe <$> Sqlite.query c
    "SELECT * FROM batons WHERE mac = ?" (Sqlite.Only mac)


--------------------------------------------------------------------------------
-- | You probably don't want to use this
deleteAll :: Database -> IO ()
deleteAll db = withConnection db $ \c -> do
    Sqlite.execute_ c "DELETE FROM laps"
    Sqlite.execute_ c "DELETE FROM teams"
    Sqlite.execute_ c "DELETE FROM stations"
    Sqlite.execute_ c "DELETE FROM batons"
