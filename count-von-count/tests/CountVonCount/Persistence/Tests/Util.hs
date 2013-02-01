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


--------------------------------------------------------------------------------
stations :: [Station]
stations =
    [ Station 1 "Pallet Town"   "00:00:00:00:00:00" 10
    , Station 2 "Viridian City" "00:00:00:00:00:01" 100
    , Station 3 "Celadon City"  "00:00:00:00:00:02" 180
    , Station 4 "Fuchsia City"  "00:00:00:00:00:03" 320
    ]


--------------------------------------------------------------------------------
batons :: [Baton]
batons =
    [ Baton 1 "00:00:00:00:01:00" 1
    , Baton 2 "00:00:00:00:01:01" 2
    , Baton 3 "00:00:00:00:01:02" 3
    ]


--------------------------------------------------------------------------------
testDatabase :: (Database -> IO a) -> IO a
testDatabase f = do
    db <- newDatabase ":memory:"

    forM_ stations $ \(Station _ name mac pos) -> addStation db name mac pos
    forM_ batons $ \(Baton _ mac nr) -> addBaton db mac nr

    r <- f db
    closeDatabase db
    return r
