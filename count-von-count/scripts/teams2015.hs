--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import CountVonCount.Persistence


--------------------------------------------------------------------------------
main :: IO ()
main = do
    db <- newDatabase "count-von-count.db"
    deleteAll db

    addStation db "gyrid101" "00:24:01:eb:c2:68" 20
    addStation db "gyrid102" "00:21:91:f1:f1:39" 70
    addStation db "gyrid103" "00:21:91:f1:f1:33" 100
    addStation db "gyrid104" "00:01:95:0b:ac:02" 150
    addStation db "gyrid105" "00:01:95:0a:56:32" 290
    addStation db "gyrid106" "00:21:91:f1:ea:11" 320

    t1  <- addTeam db "HILOK"
    t2  <- addTeam db "VGK"
    t3  <- addTeam db "VDK"
    t4  <- addTeam db "VBK"
    t5  <- addTeam db "VEK"
    t6  <- addTeam db "Wetenschappen & VLAK"
    t7  <- addTeam db "Veto, LILA, Hermes"
    t8  <- addTeam db "Blandinia"
    t9  <- addTeam db "VPPK"
    t10 <- addTeam db "Moeder Lies"
    t11 <- addTeam db "Politeia"
    t12 <- addTeam db "Kofschipclubs"
    t13 <- addTeam db "SK"
    t14 <- addTeam db "HK"
    t15 <- addTeam db "KVHV"
    t16 <- addTeam db "VRG & Farma"
    t17 <- addTeam db "VTK"
    t18 <- addTeam db "VLK"
    t19 <- addTeam db "Schamper"

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
    setTeamBaton db t18 $ Just b18
    setTeamBaton db t19 $ Just b19


    closeDatabase db

