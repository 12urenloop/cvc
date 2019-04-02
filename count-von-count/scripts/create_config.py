import string

year = input("Year: ")
with open(f"teams{year}.hs", "w") as team_file:
    team_file.write("""
    {-# LANGUAGE OverloadedStrings #-}
    import CountVonCount.Persistence

    main :: IO ()
    main = do
        db <- newDatabase "count-von-count.db"
        deleteAll db

    """)
    
    num_stations = int(input("Number of stations: "))
    for i in range(num_stations):
        print(f"Station {i+1}")
        station_name = input("Name: ")
        station_mac = input("MAC: ")
        station_distance = input("Distance from start: ")
        team_file.write(f"\taddStation db \"{station_name}\" \"{station_mac}\" {station_distance}\n")

    num_teams = int(input("Number of teams:"))
    teams = []
    for i in range(num_teams):
        print(f"Team {i+1}")
        team_name = input("Name: ")
        teams.append(team_name)
        team_file.write(f"\tt{i+1} <- addTeam db \"{team_name}\"\n")

    num_batons = int(input("Number of batons:"))
    j = 0
    batons = []
    for i in range(num_batons):
        print(f"Baton {i+1}")
        baton_name_default = "Baton " + string.ascii_uppercase[j]
        baton_name = input(f"Name (default: {baton_name_default}): ")
        if baton_name == "":
            baton_name = baton_name_default
            j += 1
        baton_mac = input("MAC: ")
        batons.append(baton_name)
        team_file.write(f"\tb{i+1} <- addBaton db \"{baton_mac}\" \"{baton_name}\"\n")

    for i, baton_name in enumerate(batons):
        print(f"{i+1}: {baton_name}")

    for i, team_name in enumerate(teams):
        baton_team_num = int(input(f"Baton number for team {team_name}: "))
        team_file.write(f"\tsetTeamBaton db t{i+1} $ Just b{baton_team_num}\n")
    team_file.write("\tcloseDatabase db\n")
