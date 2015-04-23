import json

class team:
    shortestlaptime = ''

    def __init__(self,id,name):
        self.teamid = teamid
        self.name = name
        self.shortestLap = ''
        self.distance = ''
        self.laps

    def get_distance():
        return laps * 384


#krijg statejson

with open('data.json') as data_file:    
    data = json.load(data_file)

teamDict = {}
for teamJson in data["teams"]:
    teamDict[teamJson['"id"']] = teamjson["name"]


totalDist = ''
shortestLapGlobal = ''
totalLaps = ''


#krijg lapjson
#teamlijst[  #gesorteerd op positie
#   team[
#       shortestlaptime,
#       pos,
#   ]
#]

#pas triggeren als 10 team 15 rondjes gelopen hebben
def teamPassed(team1,team2):
    pass
# TEAM1 heeft net TEAM2 ingehaald

#pas triggeren als TEAM 15 rondjes gelopen heeft
def newShortestLap(team,time):
    pass
# TEAM heeft hun laatste rondje in TIME gelopen. Dat is hun nieuwe snelste tijd!

#pas triggeren als 10 teams 15 rondjes gelopen hebben
def newShortestLapGlobal(team, time):
    pass
# :tada::tada: TEAM heeft hun laatste rondje in TIME gelopen. Dat is het snelste rondje van de dag! :tada::tada:

def lapMilestone(team, laps):
    pass
# TEAM heeft LAPS rondjes gelopen.

def tweet(msg):
    pass

