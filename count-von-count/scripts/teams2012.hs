--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import CountVonCount.Persistence


--------------------------------------------------------------------------------
main :: IO ()
main = do
    db <- newDatabase "count-von-count.db"
    deleteAll db

    addStation db "gyrid-6" "00:24:01:EB:C2:65" 0
    addStation db "gyrid-2" "00:21:91:F1:EA:0A" 77
    addStation db "gyrid-3" "00:22:B0:D0:61:1F" 143
    addStation db "gyrid-4" "00:21:91:F1:EA:11" 195
    addStation db "gyrid-5" "00:21:91:F1:EA:0E" 289

    t1  <- addTeam db "HomeKonvent"
    t2  <- addTeam db "SeniorenKonvent"
    t3  <- addTeam db "Kofschipclubs"
    t4  <- addTeam db "VEK"
    t5  <- addTeam db "VTK"
    t6  <- addTeam db "VLK"
    t7  <- addTeam db "Blandinia"
    t8  <- addTeam db "Politeia"
    t9  <- addTeam db "VRG"
    t10 <- addTeam db "Wetenschappen & Vlak"
    t11 <- addTeam db "VPPK"
    t12 <- addTeam db "VGK & GFK & VBK"
    t13 <- addTeam db "KVHV"
    t14 <- addTeam db "HILOK"

    b1  <- addBaton db "20:11:02:15:01:80" 1
    b2  <- addBaton db "20:11:02:15:01:42" 2
    b3  <- addBaton db "20:11:02:15:01:15" 3
    b4  <- addBaton db "20:11:02:15:01:75" 4
    b5  <- addBaton db "20:11:02:15:01:77" 5
    b6  <- addBaton db "20:11:02:15:01:51" 6
    b7  <- addBaton db "20:11:02:15:01:93" 7
    b8  <- addBaton db "00:12:02:01:07:82" 8
    b9  <- addBaton db "00:12:02:01:00:04" 9
    b10 <- addBaton db "20:11:02:15:01:17" 10
    b11 <- addBaton db "00:12:02:01:06:25" 11
    b12 <- addBaton db "20:11:02:15:01:50" 12
    b13 <- addBaton db "00:12:02:01:08:76" 13
    b14 <- addBaton db "20:11:02:15:01:34" 14
    b15 <- addBaton db "20:11:02:15:01:90" 15
    b16 <- addBaton db "00:11:72:14:01:98" 16
    b17 <- addBaton db "20:11:02:15:01:67" 17
    b18 <- addBaton db "20:11:02:15:01:83" 18
    b19 <- addBaton db "20:11:02:15:01:79" 19

    setTeamBaton db t1  $ Just b5
    setTeamBaton db t2  $ Just b15
    setTeamBaton db t3  $ Just b2
    setTeamBaton db t4  $ Just b9
    setTeamBaton db t5  $ Just b11
    setTeamBaton db t6  $ Just b8
    setTeamBaton db t7  $ Just b17
    setTeamBaton db t8  $ Just b7
    setTeamBaton db t9  $ Just b10
    setTeamBaton db t10 $ Just b13
    setTeamBaton db t11 $ Just b16
    setTeamBaton db t12 $ Just b18
    setTeamBaton db t13 $ Just b19
    setTeamBaton db t14 $ Just b14

    closeDatabase db
