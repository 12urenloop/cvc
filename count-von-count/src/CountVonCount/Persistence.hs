{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module CountVonCount.Persistence
    ( module CountVonCount.Persistence.Core
    , Lap (..)
    , Team (..)
    , addLap
    , addLaps
    , getTeamByMac
    , getLaps
    ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Text as T
import qualified Database.MongoDB as MDB

import CountVonCount.Types
import CountVonCount.Persistence.Core

data Lap = Lap
    { lapTeam      :: Ref Team
    , lapTimestamp :: UTCTime
    , lapReason    :: Text
    , lapCount     :: Int
    } deriving (Show)

instance IsDocument Lap where
    collection _     = "laps"
    toDocument lap   =
        [ "team"      MDB.:= lapTeam lap
        , "timestamp" MDB.=: lapTimestamp lap
        , "reason"    MDB.=: T.unpack (lapReason lap)
        , "count"     MDB.=: lapCount lap
        ]
    fromDocument doc = Lap
        (MDB.valueAt "team" doc)
        (MDB.at "timestamp" doc)
        (T.pack $ MDB.at "reason" doc)
        (MDB.at "count" doc)

data Team = Team
    { teamId    :: Int
    , teamName  :: Text
    , teamLaps  :: Int
    , teamBaton :: Maybe Mac
    } deriving (Eq, Ord)

instance Show Team where
    show = T.unpack . teamName

instance ToJSON Team where
    toJSON (Team id' name laps baton) = object
        ["id" .= id', "name" .= name, "laps" .= laps, "baton" .= baton]

instance IsDocument Team where
    collection _     = "teams"
    toDocument team  =
        [ "id"    MDB.=: teamId team
        , "name"  MDB.=: T.unpack (teamName team)
        , "laps"  MDB.=: teamLaps team
        , "baton" MDB.=: fmap T.unpack (teamBaton team)
        ]
    fromDocument doc = Team
        (MDB.at "id" doc)
        (T.pack $ MDB.at "name" doc)
        (MDB.at "laps" doc)
        (fmap T.pack $ MDB.at "baton" doc)

addLap :: Ref Team -> UTCTime -> Persistence ()
addLap team timestamp = addLaps team timestamp "counted lap" 1

addLaps :: Ref Team -> UTCTime -> Text -> Int -> Persistence ()
addLaps ref timestamp reason c = do
    team <- get ref
    add $ Lap ref timestamp reason c
    put ref $ team {teamLaps = teamLaps team + c}

getTeamByMac :: Mac -> Persistence (Maybe (Ref Team, Team))
getTeamByMac m = do
    cursor <- MDB.find $ MDB.select ["baton" MDB.=: T.unpack m] $ collection x
    docs   <- MDB.rest cursor
    return $ case docs of
        [doc] -> Just (MDB.valueAt "_id" doc, fromDocument doc)
        _     -> Nothing
  where
    x = undefined :: Team

getLaps :: Int                -- ^ Offset (0-based)
        -> Int                -- ^ Count
        -> Persistence [Lap]  -- ^ Matching laps
getLaps offset count = do
    cursor <- MDB.find (MDB.select [] $ collection x)
        { MDB.skip  = fromIntegral offset
        , MDB.limit = fromIntegral count
        , MDB.sort  = ["timestamp" MDB.=: (-1 :: Int)]
        }
    docs   <- MDB.rest cursor
    return $ map fromDocument docs
  where
    x = undefined :: Lap
