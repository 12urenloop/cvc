{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module CountVonCount.Persistence
    ( module CountVonCount.Persistence.Core
    , Team (..)
    ) where

import qualified Database.MongoDB as MDB

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
