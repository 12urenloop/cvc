--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           CountVonCount.Persistence


--------------------------------------------------------------------------------
main :: IO ()
main = do
    db <- newDatabase "count-von-count.db"
    deleteAll db

    addStation db "station 1" "00:00:00:00:01:00" 0
    addStation db "station 2" "00:00:00:00:02:00" 100
    addStation db "station 3" "00:00:00:00:03:00" 200
    addStation db "station 4" "00:00:00:00:04:00" 300

    t1 <- addTeam db "Bulbasaur"
    t2 <- addTeam db "Machop"
    t3 <- addTeam db "Mankey"
    t4 <- addTeam db "Charmander"

    b1 <- addBaton db "20:11:02:15:01:80" "Baton A"
    b2 <- addBaton db "20:11:02:15:01:42" "Baton B"
    b3 <- addBaton db "20:11:02:15:01:15" "Baton C"
    b4 <- addBaton db "20:11:02:15:01:75" "Baton D"
    _  <- addBaton db "20:11:02:15:01:77" "Baton E"
    _  <- addBaton db "20:11:02:15:01:51" "Baton F"
    _  <- addBaton db "20:11:02:15:01:93" "Baton G"
    _  <- addBaton db "00:12:02:01:07:82" "Baton H"
    _  <- addBaton db "00:12:02:01:00:04" "Baton I"
    _  <- addBaton db "20:11:02:15:01:17" "Baton J"
    _  <- addBaton db "00:12:02:01:06:25" "Baton K"
    _  <- addBaton db "20:11:02:15:01:50" "Baton L"
    _  <- addBaton db "00:12:02:01:08:76" "Baton M"
    _  <- addBaton db "20:11:02:15:01:34" "Baton N"
    _  <- addBaton db "20:11:02:15:01:90" "Baton O"
    _  <- addBaton db "00:11:72:14:01:98" "Baton P"
    _  <- addBaton db "20:11:02:15:01:67" "Baton Q"
    _  <- addBaton db "20:11:02:15:01:83" "Baton R"
    _  <- addBaton db "20:11:02:15:01:79" "Baton S"

    setTeamBaton db t1 $ Just b1
    setTeamBaton db t2 $ Just b2
    setTeamBaton db t3 $ Just b3
    setTeamBaton db t4 $ Just b4

    closeDatabase db
