--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.Persistence.Tests.Util
    ( stations
    , batons
    , testDatabase
    ) where


--------------------------------------------------------------------------------
import           Control.Monad             (forM_)


--------------------------------------------------------------------------------
import           CountVonCount.Persistence
import           CountVonCount.Types


--------------------------------------------------------------------------------
stations :: [Station]
stations =
    [ Station 1 "Pallet Town"   "00:00:00:00:00:00" 0
    , Station 2 "Viridian City" "00:00:00:00:00:01" 100
    , Station 3 "Celadon City"  "00:00:00:00:00:02" 200
    , Station 4 "Fuchsia City"  "00:00:00:00:00:03" 300
    ]


--------------------------------------------------------------------------------
batons :: [Baton]
batons =
    [ Baton "00:00:00:00:01:00" 1
    , Baton "00:00:00:00:01:01" 2
    , Baton "00:00:00:00:01:02" 3
    ]


--------------------------------------------------------------------------------
testDatabase :: (Database -> IO a) -> IO a
testDatabase f = do
    db <- newDatabase ":memory:"

    forM_ stations $ \(Station _ name mac pos) -> addStation db name mac pos

    r <- f db
    closeDatabase db
    return r
