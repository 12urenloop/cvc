{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module CountVonCount.Persistence
    ( module CountVonCount.Persistence.Core
    , Team (..)
    , getTeamByMac
    ) where

import qualified Data.ByteString.Char8 as BC
import qualified Database.MongoDB as MDB

import CountVonCount.Types
import CountVonCount.Persistence.Core

data Team = Team
    { teamName  :: String
    , teamLaps  :: Int
    , teamBaton :: Maybe String
    } deriving (Eq, Show)

instance IsDocument Team where
    collection _     = "teams"
    toDocument team  =
        [ "name"  MDB.=: teamName team
        , "laps"  MDB.=: teamLaps team
        , "baton" MDB.=: teamBaton team
        ]
    fromDocument doc = Team
        (MDB.at "name" doc)
        (MDB.at "laps" doc)
        (MDB.at "baton" doc)

getTeamByMac :: Mac -> Persistence (Maybe (Ref Team, Team))
getTeamByMac m = do
    cursor <- MDB.find $ MDB.select ["baton" MDB.=: BC.unpack m] $ collection x
    docs   <- MDB.rest cursor
    return $ case docs of
        [doc] -> Just (MDB.valueAt "_id" doc, fromDocument doc)
        _     -> Nothing
  where
    x = undefined :: Team
