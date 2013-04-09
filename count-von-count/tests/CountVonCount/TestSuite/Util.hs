--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module CountVonCount.TestSuite.Util
    ( testStations
    , testBatons
    , testDatabase
    , testLog
    ) where


--------------------------------------------------------------------------------
import           Control.Monad             (forM_)


--------------------------------------------------------------------------------
import           CountVonCount.Log         (Log)
import qualified CountVonCount.Log         as Log
import           CountVonCount.Persistence


--------------------------------------------------------------------------------
testStations :: [Station]
testStations =
    [ Station 1 "Pallet Town"   "00:00:00:00:00:00" 10
    , Station 2 "Viridian City" "00:00:00:00:00:01" 100
    , Station 3 "Celadon City"  "00:00:00:00:00:02" 180
    , Station 4 "Fuchsia City"  "00:00:00:00:00:03" 320
    ]


--------------------------------------------------------------------------------
testBatons :: [Baton]
testBatons =
    [ Baton 1 "00:00:00:00:01:00" "Baton A"
    , Baton 2 "00:00:00:00:01:01" "Baton B"
    , Baton 3 "00:00:00:00:01:02" "Baton C"
    ]


--------------------------------------------------------------------------------
testDatabase :: (Database -> IO a) -> IO a
testDatabase f = do
    db <- newDatabase ":memory:"

    forM_ testStations $ \(Station _ name mac pos) -> addStation db name mac pos
    forM_ testBatons $ \(Baton _ mac nr) -> addBaton db mac nr

    r <- f db
    closeDatabase db
    return r


--------------------------------------------------------------------------------
testLog :: (Log -> IO a) -> IO a
testLog f = do
    logger <- Log.open "/dev/null" False
    x      <- f logger
    Log.close logger
    return x
