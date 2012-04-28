{-# LANGUAGE BangPatterns, DeriveDataTypeable, OverloadedStrings,
        ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Persistence
    ( Persistence
    , runPersistence

    , PersistenceException (..)

    , Ref
    , refToString
    , refFromString

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

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Exception (Exception, IOException, handle, throw)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Ord (comparing)
import Data.Typeable (Typeable)

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Text as T
import qualified Database.MongoDB as MDB

import CountVonCount.Types

type Persistence = MDB.Action IO

runPersistence :: MonadIO m => Persistence a -> m a
runPersistence p = liftIO $ do
    -- TODO: pool and re-use connections
    pipe <- handle connect $ MDB.runIOE $ MDB.connect $ MDB.host "127.0.0.1"
    handle query $ do
        x    <- MDB.access pipe MDB.master "count-von-count" p
        MDB.close pipe
        return $ either (error . show) id x
  where
    connect, query :: IOException -> IO a
    connect _ = throw $ PersistenceException "Can't connect to MongoDB"
    query   _ = throw $ PersistenceException "Can't execute MongoDB-query"

data PersistenceException = PersistenceException String
    deriving (Show, Typeable)

instance Exception PersistenceException

type Ref a = MDB.Value

refToString :: Ref a -> String
refToString = show

refFromString :: String -> Ref a
refFromString = MDB.ObjId . read

class FromDocument a where
    fromDocument :: MDB.Document -> a

data Team = Team
    { teamId    :: Text
    , teamName  :: Text
    , teamLaps  :: Int
    , teamBaton :: Maybe Mac
    }

instance Eq Team where
    t1 == t2 = teamId t1 == teamId t2

instance Ord Team where
    compare = comparing teamId

instance Show Team where
    show = T.unpack . teamName

instance ToJSON Team where
    toJSON (Team id' name laps baton) = object
        ["id" .= id', "name" .= name, "laps" .= laps, "baton" .= baton]

instance FromDocument Team where
    fromDocument doc = Team
        (T.pack $ MDB.at "id" doc)
        (T.pack $ MDB.at "name" doc)
        (MDB.at "laps" doc)
        (fmap T.pack $ MDB.at "baton" doc)

-- TODO: Perhaps only take name as an argument, deduce the rest?
addTeam :: Team -> Persistence (Ref Team)
addTeam team = MDB.insert "teams"
    [ "id"    MDB.=: T.unpack (teamId team)
    , "name"  MDB.=: T.unpack (teamName team)
    , "laps"  MDB.=: teamLaps team
    , "baton" MDB.=: fmap T.unpack (teamBaton team)
    ]

getTeam :: Ref Team -> Persistence Team
getTeam ref = fromDocument <$>
    MDB.fetch (MDB.select ["_id" MDB.:= ref] "teams")

getTeamByMac :: Mac -> Persistence (Maybe (Ref Team, Team))
getTeamByMac m = do
    cursor <- MDB.find $ MDB.select ["baton" MDB.=: T.unpack m] "teams"
    docs   <- MDB.rest cursor
    return $ case docs of
        [doc] -> Just (MDB.valueAt "_id" doc, fromDocument doc)
        _     -> Nothing

getAllTeams :: Persistence [(Ref Team, Team)]
getAllTeams = do
    cursor <- MDB.find $ MDB.select [] "teams"
    docs   <- MDB.rest cursor
    return $ map (MDB.valueAt "_id" &&& fromDocument) docs

setTeamBaton :: Ref Team -> Maybe Baton -> Persistence ()
setTeamBaton ref baton = MDB.modify
    (MDB.select ["_id" MDB.:= ref] "teams")
    ["$set" MDB.=: ["baton" MDB.=: fmap (T.unpack . batonMac) baton]]

data Lap = Lap
    { lapTimestamp :: UTCTime
    , lapReason    :: Text
    , lapCount     :: Int
    } deriving (Show)

instance FromDocument Lap where
    fromDocument doc = Lap
        (MDB.at "timestamp" doc)
        (T.pack $ MDB.at "reason" doc)
        (MDB.at "count" doc)

addLap :: Ref Team -> UTCTime -> Persistence Team
addLap ref timestamp = addLaps ref timestamp "Full lap detected" 1

addLaps :: Ref Team -> UTCTime -> Text -> Int -> Persistence Team
addLaps !ref !timestamp !reason !c = MDB.modify
    (MDB.select ["_id" MDB.:= ref] "teams")
    [ "$inc"  MDB.=: ["laps" MDB.=: c]
    , "$push" MDB.=: ["laps_" MDB.=: lapDoc]
    ] >> getTeam ref
  where
    lapDoc =
        [ "timestamp" MDB.=: timestamp
        , "reason"    MDB.=: T.unpack reason
        , "count"     MDB.=: c
        ]

getLatestLaps :: Ref Team -> Int -> Persistence [Lap]
getLatestLaps !ref n = do
    -- NOTE: I have to use the project modifier here, because there is no other
    -- way for me to specify this
    team <- MDB.fetch $ (MDB.select ["_id" MDB.:= ref] "teams")
        {MDB.project = ["laps_" MDB.=: ["$slice" MDB.=: -n]]}
    let laps = MDB.at "laps_" team
    return $ fmap fromDocument laps

-- | You probably don't want to use this
deleteAll :: Persistence ()
deleteAll = do
    _ <- MDB.dropCollection "teams"
    return ()

