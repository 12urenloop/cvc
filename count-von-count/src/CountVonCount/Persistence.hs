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
    , getLatestLaps

    , deleteAll
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative    ((<$>), (<*>))
import           Data.Aeson             (ToJSON (..), object, (.=))
import           Data.Int               (Int64)
import           Data.Maybe             (listToMaybe)
import           Data.Ord               (comparing)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (UTCTime)
import           Database.SQLite.Simple (FromRow (..))
import qualified Database.SQLite.Simple as Sqlite


--------------------------------------------------------------------------------
import           CountVonCount.Types


--------------------------------------------------------------------------------
newtype Database = Database
    { unDatabase :: Sqlite.Connection
    }


--------------------------------------------------------------------------------
newDatabase :: FilePath -> IO Database
newDatabase fp = do
    c <- Sqlite.open fp
    Sqlite.execute_ c teamTable
    Sqlite.execute_ c lapsTable
    return $ Database c


--------------------------------------------------------------------------------
closeDatabase :: Database -> IO ()
closeDatabase (Database c) = Sqlite.close c


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
    , teamBaton :: Maybe Mac
    }


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
instance ToJSON Team where
    toJSON (Team id' name laps baton) = object
        ["id" .= id', "name" .= name, "laps" .= laps, "baton" .= baton]


--------------------------------------------------------------------------------
instance FromRow Team where
    fromRow = Team
        <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field <*> Sqlite.field


--------------------------------------------------------------------------------
teamTable :: Sqlite.Query
teamTable =
    "CREATE TABLE IF NOT EXISTS teams ( \
    \    id INTEGER PRIMARY KEY,        \
    \    name TEXT,                     \
    \    laps INTEGER,                  \
    \    baton TEXT                     \
    \)"


--------------------------------------------------------------------------------
addTeam :: Database -> Text -> IO (Ref Team)
addTeam (Database c) name = do
    Sqlite.execute c "INSERT INTO teams (name, laps, baton) VALUES (?, ?, ?)"
        (name, 0 :: Int, Nothing :: Maybe Mac)
    Sqlite.lastInsertRowId c


--------------------------------------------------------------------------------
getTeam :: Database -> Ref Team -> IO Team
getTeam (Database c) ref = do
    teams <- Sqlite.query c "SELECT * FROM teams WHERE id = ?" (Sqlite.Only ref)
    case teams of
        (x : _) -> return x
        []      -> error $ "No team with ID " ++ refToString ref


--------------------------------------------------------------------------------
getTeamByMac :: Database -> Mac -> IO (Maybe Team)
getTeamByMac (Database c) mac = listToMaybe <$>
    Sqlite.query c "SELECT * FROM teams WHERE baton = ?" (Sqlite.Only mac)


--------------------------------------------------------------------------------
getAllTeams :: Database -> IO [Team]
getAllTeams (Database c) = Sqlite.query_ c "SELECT * FROM teams"


--------------------------------------------------------------------------------
setTeamBaton :: Database -> Ref Team -> Maybe Baton -> IO ()
setTeamBaton (Database c) ref baton = Sqlite.execute c
    "UPDATE teams SET baton = ? WHERE id = ?"
    (fmap batonMac baton, ref)


--------------------------------------------------------------------------------
data Lap = Lap
    { lapId        :: Ref Lap
    , lapTeam      :: Ref Team
    , lapTimestamp :: UTCTime
    , lapReason    :: Text
    , lapCount     :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
instance FromRow Lap where
    fromRow = Lap
        <$> Sqlite.field <*> Sqlite.field <*> Sqlite.field
        <*> Sqlite.field <*> Sqlite.field


--------------------------------------------------------------------------------
lapsTable :: Sqlite.Query
lapsTable =
    "CREATE TABLE IF NOT EXISTS laps (             \
    \    id INTEGER PRIMARY KEY,                   \
    \    team_id INTEGER,                          \
    \    timestamp TIMESTAMP,                      \
    \    reason TEXT,                              \
    \    count INTEGER,                            \
    \    FOREIGN KEY(team_id) REFERENCES team (id) \
    \)"


--------------------------------------------------------------------------------
addLap :: Database -> Ref Team -> UTCTime -> IO Team
addLap db ref timestamp = addLaps db ref timestamp "Full lap detected" 1


--------------------------------------------------------------------------------
addLaps :: Database -> Ref Team -> UTCTime -> Text -> Int -> IO Team
addLaps db !ref !timestamp !reason !count = do
    Sqlite.execute (unDatabase db)
        "INSERT INTO laps (team_id, timestamp, reason, count) \
        \VALUES (?, ?, ?, ?)"
        (ref, timestamp, reason, count)
    Sqlite.execute (unDatabase db)
        "UPDATE teams SET laps = laps + ? WHERE id = ?"
        (count, ref)
    getTeam db ref


--------------------------------------------------------------------------------
getLatestLaps :: Database -> Ref Team -> Int -> IO [Lap]
getLatestLaps (Database c) !ref n =
    Sqlite.query c
        "SELECT * FROM laps WHERE team_id = ? ORDER BY id DESC LIMIT ?"
        (ref, n)


--------------------------------------------------------------------------------
-- | You probably don't want to use this
deleteAll :: Database -> IO ()
deleteAll (Database c) = do
    Sqlite.execute_ c "DELETE FROM laps"
    Sqlite.execute_ c "DELETE FROM teams"
