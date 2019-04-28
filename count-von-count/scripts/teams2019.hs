{-# LANGUAGE OverloadedStrings #-}

import CountVonCount.Persistence

main :: IO ()
main = do
    db <- newDatabase "count-von-count.db"
    deleteAll db

    -- Define stations: gyrid name and MAC-address of its bluetooth stick and distance
    -- TODO: Fix MAC addresses and distances
    addStation db "mr-brown" "00:01:95:0d:cf:d7" 1
    addStation db "mr-white" "00:01:95:0d:d1:f0" 48
    addStation db "mr-blonde" "00:01:95:0a:56:3b" 111
    addStation db "mr-orange" "00:01:95:0a:cc:3f" 185
    addStation db "mr-blue" "00:01:95:0d:cf:e4" 230

    -- Add teams --------------
    t1 <- addTeam db "VTK"
    t2 <- addTeam db "HILOK"
    t3 <- addTeam db "VLK"
    t4 <- addTeam db "VEK & Moeder Lies"
    t5 <- addTeam db "Wetenschappen"
    t6 <- addTeam db "SK"
    t7 <- addTeam db "VGK"
    t8 <- addTeam db "HK"
    t9 <- addTeam db "Politea"
    t10 <- addTeam db "VRG"
    t11 <- addTeam db "Blandinia"
    t12 <- addTeam db "VPPK"
    t13 <- addTeam db "WVK"
    t14 <- addTeam db "Hermes & Veto & LILA"
    t15 <- addTeam db "Lombrosiana & VBK"
    t16 <- addTeam db "Antilopen"

    -- Add batons ---------------------------------
    b1 <- addBaton db "20:13:01:30:03:69" "Baton B"
    b2 <- addBaton db "20:13:12:06:90:51" "Baton C"
    b3 <- addBaton db "20:13:01:24:10:46" "Baton E"
    b4 <- addBaton db "20:13:01:24:00:43" "Baton F"
    b5 <- addBaton db "00:11:11:22:07:59" "Baton G"
    b6 <- addBaton db "20:13:02:18:02:86" "Baton H"
    b7 <- addBaton db "20:13:02:19:15:17" "Baton I"
    b8 <- addBaton db "20:13:01:31:01:32" "Baton J"
    b9 <- addBaton db "00:13:12:06:43:71" "Baton K"
    b10 <- addBaton db "20:13:01:24:00:28" "Baton L"
    b11 <- addBaton db "00:00:12:06:61:63" "Baton P"
    b12 <- addBaton db "00:12:02:01:08:76" "Baton Q"
    b13 <- addBaton db "20:11:02:15:01:93" "Baton R"
    b14 <- addBaton db "20:11:02:15:01:77" "Baton T"
    b15 <- addBaton db "20:13:12:05:07:89" "Baton U"
    b16 <- addBaton db "00:13:12:06:50:94" "Baton V"
    b17 <- addBaton db "00:12:02:01:06:25" "Baton W"
    b18 <- addBaton db "20:11:02:15:01:79" "Baton Y"
    b19 <- addBaton db "20:13:12:06:90:49" "Baton Z"

    -- b1 <- addBaton db "GEEN BEDRADING" "Baton A"
    -- b4 <- addBaton db "GEEN BATTERIJEN" "Baton D"
    -- b13 <- addBaton db "GEEN BEDRADING" "Baton M"
    -- b14 <- addBaton db "DRAAD GEBROKEN" "Baton N"
    -- b15 <- addBaton db "GEEN BATTERIJ BEDRADING" "Baton O"
    -- b19 <- addBaton db "KAPOTTE BATTERIJ BEDRADING" "Baton S"
    -- b24 <- addBaton db "KWIJT" "Baton X"


    setTeamBaton db t1 $ Just b1
    setTeamBaton db t2 $ Just b2
    setTeamBaton db t3 $ Just b3
    setTeamBaton db t4 $ Just b4
    setTeamBaton db t5 $ Just b5
    setTeamBaton db t6 $ Just b6
    setTeamBaton db t7 $ Just b7
    setTeamBaton db t8 $ Just b8
    setTeamBaton db t9 $ Just b9
    setTeamBaton db t10 $ Just b10
    setTeamBaton db t11 $ Just b11
    setTeamBaton db t12 $ Just b12
    setTeamBaton db t13 $ Just b13
    setTeamBaton db t14 $ Just b14
    setTeamBaton db t15 $ Just b15
    setTeamBaton db t16 $ Just b16


    -- Spare working batons
    -- setTeamBaton db t17 $ Just b17
    -- setTeamBaton db t18 $ Just b18
    -- setTeamBaton db t19 $ Just b19
