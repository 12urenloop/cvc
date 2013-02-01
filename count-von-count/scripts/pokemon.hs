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

    b1 <- addBaton db "20:11:02:15:01:80" 1
    b2 <- addBaton db "20:11:02:15:01:42" 2
    b3 <- addBaton db "20:11:02:15:01:15" 3
    b4 <- addBaton db "20:11:02:15:01:75" 4
    _  <- addBaton db "20:11:02:15:01:77" 5
    _  <- addBaton db "20:11:02:15:01:51" 6
    _  <- addBaton db "20:11:02:15:01:93" 7
    _  <- addBaton db "00:12:02:01:07:82" 8
    _  <- addBaton db "00:12:02:01:00:04" 9
    _  <- addBaton db "20:11:02:15:01:17" 10
    _  <- addBaton db "00:12:02:01:06:25" 11
    _  <- addBaton db "20:11:02:15:01:50" 12
    _  <- addBaton db "00:12:02:01:08:76" 13
    _  <- addBaton db "20:11:02:15:01:34" 14
    _  <- addBaton db "20:11:02:15:01:90" 15
    _  <- addBaton db "00:11:72:14:01:98" 16
    _  <- addBaton db "20:11:02:15:01:67" 17
    _  <- addBaton db "20:11:02:15:01:83" 18
    _  <- addBaton db "20:11:02:15:01:79" 19

    setTeamBaton db t1 $ Just b1
    setTeamBaton db t2 $ Just b2
    setTeamBaton db t3 $ Just b3
    setTeamBaton db t4 $ Just b4

    closeDatabase db
