import json

OMTREK = 384

class Team:

    def __init__(self,name):
        self.name = name
        self.shortestLap = sys.maxint #in milliseconds
        self.lastLapTimeStamp

#krijg statejson

with open('statejson.json') as data_file:
    state = json.load(data_file)

teams = []
for teamjson in state["teams"]:
    teams[teamjson["id"]] = Team(teamjson["name"])


shortestLapGlobal = sys.maxint #in milliseconds
totalLaps = 0

#krijg lapjson

#update teamstata
team = teams[lap["id"]]
team.laps = lap["total"]
totalLaps = lap["id"]

#check teamtriggers
laptime = lap["timestamp"] - team.lastLapTimeStamp
if laptime < team.shortestLap and team.laps > 10:
    team.shortestLap = laptime
    #TRIGGER SHORTESTLAP

if team.laps % 100 == 0:
    #TRIGGER LAPMILESTONE

#check globaltriggers
if laptime < shortestLapGlobal and totalLaps > 100:
    shortestLapGlobal = laptime
    #TRIGGER SHORTESTLAPGLOBAL

distance = totalLaps * OMTREK
if distance

team.lastLapTimeStamp = lap["timestamp"]


#pas triggeren als 10 team 15 rondjes gelopen hebben
# TEAM1 heeft net TEAM2 ingehaald

#pas triggeren als TEAM 15 rondjes gelopen heeft
# TEAM heeft hun laatste rondje in TIME gelopen. Dat is hun nieuwe snelste tijd!

#pas triggeren als 10 teams 15 rondjes gelopen hebben
# :tada::tada: TEAM heeft hun laatste rondje in TIME gelopen. Dat is het snelste rondje van de dag! :tada::tada:

# TEAM heeft LAPS rondjes gelopen.

def tweet(msg):
    pass
