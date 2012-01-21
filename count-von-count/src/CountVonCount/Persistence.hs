{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module CountVonCount.Persistence
    ( module CountVonCount.Persistence.Core
    , Team (..)
    , Baton (..)
    , batonName
    ) where

import qualified Database.MongoDB as MDB

import CountVonCount.Persistence.Core

data Team = Team
    { teamName  :: String
    , teamLaps  :: Int
    , teamBaton :: Maybe (Ref Baton)
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

data Baton = Baton
    { batonNr   :: Int
    , batonMac  :: String
    , batonTeam :: Maybe (Ref Team)
    } deriving (Eq, Show)

instance IsDocument Baton where
    collection _     = "batons"
    toDocument baton =
        [ "nr"   MDB.=: batonNr baton
        , "mac"  MDB.=: batonMac baton
        , "team" MDB.=: batonTeam baton
        ]
    fromDocument doc            = Baton
        (MDB.at "nr" doc)
        (MDB.at "mac" doc)
        (MDB.at "team" doc)

batonName :: Baton -> String
batonName = ("Baton " ++) . show . batonNr
