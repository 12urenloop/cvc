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

    t1  <- addTeam db "KVHV"
    t2  <- addTeam db "VRG"
    t3  <- addTeam db "VEK"
    t4  <- addTeam db "Blandinia"
    t5  <- addTeam db "Wetenschappen & VLAK"
    t6  <- addTeam db "VLK"
    t7  <- addTeam db "VTK"
    t8  <- addTeam db "VPPK"
    t9  <- addTeam db "HILOK"
    t10 <- addTeam db "FARMA & VGK & DENTALIA & VBK"
    t11 <- addTeam db "SK"
    t12 <- addTeam db "HK"
    t13 <- addTeam db "Politeia"
    t14 <- addTeam db "Curatio"
    t15 <- addTeam db "Kofschip"

    b1  <- addBaton db "20:13:01:24:11:15" "Baton A"
    b2  <- addBaton db "20:13:01:30:03:69" "Baton B"
    b3  <- addBaton db "20:13:01:31:03:09" "Baton C"
    b4  <- addBaton db "20:13:02:20:15:11" "Baton D"
    b5  <- addBaton db "20:13:01:24:10:46" "Baton E"
    b6  <- addBaton db "20:13:01:24:00:43" "Baton F"
    b7  <- addBaton db "20:13:02:21:01:04" "Baton G"
    b8  <- addBaton db "20:13:02:18:02:86" "Baton H"
    b9  <- addBaton db "20:13:02:19:15:17" "Baton I"
    b10 <- addBaton db "20:13:01:31:01:32" "Baton J"
    b11 <- addBaton db "20:13:02:20:16:95" "Baton K"
    b12 <- addBaton db "20:13:01:24:00:28" "Baton L"
    b13 <- addBaton db "20:11:02:15:01:90" "Baton M"
    b14 <- addBaton db "20:11:02:15:01:67" "Baton N"
    b15 <- addBaton db "20:11:02:15:01:42" "Baton O"
    b16 <- addBaton db "00:12:02:01:00:04" "Baton P"
    b17 <- addBaton db "00:12:02:01:08:76" "Baton Q"
    b18 <- addBaton db "20:11:02:15:01:93" "Baton R"
    b19 <- addBaton db "20:11:02:15:01:17" "Baton S"
    b20 <- addBaton db "20:11:02:15:01:77" "Baton T"
    b21 <- addBaton db "20:11:02:15:01:34" "Baton U"
    b22 <- addBaton db "20:11:02:15:01:83" "Baton V"
    b23 <- addBaton db "00:12:02:01:06:25" "Baton W"
    b24 <- addBaton db "00:12:02:01:07:82" "Baton X"
    b25 <- addBaton db "20:11:02:15:01:79" "Baton Y"

    setTeamBaton db t1  $ Just b1
    setTeamBaton db t2  $ Just b2
    setTeamBaton db t3  $ Just b3
    setTeamBaton db t4  $ Just b4
    setTeamBaton db t5  $ Just b5
    setTeamBaton db t6  $ Just b6
    setTeamBaton db t7  $ Just b7
    setTeamBaton db t8  $ Just b8
    setTeamBaton db t9  $ Just b9
    setTeamBaton db t10 $ Just b10
    setTeamBaton db t11 $ Just b11
    setTeamBaton db t12 $ Just b12
    setTeamBaton db t13 $ Just b13
    setTeamBaton db t14 $ Just b14
    setTeamBaton db t15 $ Just b15

    closeDatabase db
