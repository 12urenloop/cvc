--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import CountVonCount.Persistence


--------------------------------------------------------------------------------
main :: IO ()
main = do
    db <- newDatabase "count-von-count.db"
    deleteAll db

    addStation db "gyrid1" "00:1C:F0:F1:F8:25"  45
    addStation db "gyrid2" "00:24:01:EB:C2:68" 141
    addStation db "gyrid3" "00:1C:F0:6C:39:32" 220
    addStation db "gyrid4" "00:21:91:F1:F1:33" 300
    addStation db "gyrid5" "00:24:01:EB:C2:65" 365

    t1  <- addTeam db "KVHV"
    t2  <- addTeam db "Kofschipclubs"
    t3  <- addTeam db "Wetenschappen & VLAK"
    t4  <- addTeam db "Blandinia"
    t5  <- addTeam db "VEK"
    t6  <- addTeam db "VLK"
    t7  <- addTeam db "VTK"
    t8  <- addTeam db "HILOK"
    t9  <- addTeam db "VBK"
    t10 <- addTeam db "VGK"
    t11 <- addTeam db "Politeia"
    t12 <- addTeam db "VPPK"
    t13 <- addTeam db "Veto, Moeder Lies, LILA & Hermes"
    t14 <- addTeam db "VRG & Farma"
    t15 <- addTeam db "SeniorenKonvent"
    t16 <- addTeam db "Home Konvent"
    t17 <- addTeam db "Urgent & Schamper"

    b1  <- addBaton db "20:13:01:24:11:15" "Baton A"
    b2  <- addBaton db "20:13:01:30:03:69" "Baton B"
    b3  <- addBaton db "20:13:01:31:03:09" "Baton C"
    b4  <- addBaton db "20:13:02:20:15:11" "Baton D"
    b5  <- addBaton db "20:13:01:24:10:46" "Baton E"
    b6  <- addBaton db "20:13:01:24:00:43" "Baton F"
    b7  <- addBaton db "20:13:02:21:01:04" "Baton G"
    b8  <- addBaton db "00:13:12:06:52:19" "Baton H"
    b9  <- addBaton db "20:13:02:19:15:17" "Baton I"
    b10 <- addBaton db "20:13:01:31:01:32" "Baton J"
    b11 <- addBaton db "00:13:12:06:43:71" "Baton K"
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
    b22 <- addBaton db "00:13:12:06:50:94" "Baton V"
    b23 <- addBaton db "00:12:02:01:06:25" "Baton W"
    b24 <- addBaton db "00:12:02:01:07:82" "Baton X"
    b25 <- addBaton db "20:11:02:15:01:79" "Baton Y"
    b26 <- addBaton db "20:13:12:06:90:49" "Baton Z"

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
    setTeamBaton db t16 $ Just b16
    setTeamBaton db t17 $ Just b17

    closeDatabase db
