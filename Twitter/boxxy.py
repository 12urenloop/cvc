
from socketIO_client import SocketIO, LoggingNamespace
from functools import wraps
from collections import deque
#from twitter import *


DISTANCES = deque([
    (261290, 'Parijs', True),
    (318720, 'Londen', True),
    (489760, 'Hamburg', True),
    (522580, 'Parijs', False),
    (601870, 'München', True),
    (637440, 'Londen', False),
    (651620, 'Berlijn', True),
    (696610, 'Milaan', True),
    (721080, 'Praag', True),
    (766670, 'Kopenhagen', True),
    (773200, 'Dublin', True),
    (914810, 'Wenen', True),
    (979520, 'Hamburg', False),
    (1062890, 'Barcelona', True),
    (1131520, 'Boedapest', True),
    (1159850, 'Warschau', True),
    (1171340, 'Rome', True),
    (1203740, 'München', False),
    (1280880, 'Stockholm', True),
    (1303240, 'Berlijn', False),
    (1314300, 'Madrid', True),
    (1372590, 'Belgrade', True),
    (1393220, 'Milaan', False),
    (1442160, 'Praag', False),
    (1533340, 'Kopenhagen', False),
    (1546400, 'Dublin', False),
    (1697830, 'Sofia', True),
    (1769690, 'Boekarest', True),
    (1829620, 'Wenen', False),
    (1836200, 'Kiev', True),
    (1903660, 'Sint', True),
    (2125780, 'Barcelona', False),
    (2178850, 'Istanbul', True),
    (2253260, 'Moskou', True),
    (2263040, 'Boedapest', False),
    (2319700, 'Warschau', False),
    (2342680, 'Rome', False),
    (2561760, 'Stockholm', False),
    (2628600, 'Madrid', False),
    (2745180, 'Belgrade', False),
    (3395660, 'Sofia', False),
    (3539380, 'Boekarest', False),
    (3672400, 'Kiev', False),
    (3807320, 'Sint', False),
    (4357700, 'Istanbul', False),
    (4506520, 'Moskou', False),
    ])
ACCESS_KEY = ''
ACCESS_SECRET = ''
CONSUMER_KEY = ''
CONSUMER_SECRET = ''


def main():
    boxxy = Boxxy()
    socketIO = SocketIO('live.12urenloop.be', 8080, LoggingNamespace, resource='socket.io')

    paths = {
        "/ping": boxxy.ping,
        "/state": boxxy.state,
        "/lap": boxxy.lap,
        "/position": boxxy.position
        }
    for path, method in paths.items():
        socketIO.on(path, method)

    socketIO.wait(seconds=3600*12)


class Team:

    def __init__(self,name):
        self.name = name
        self.shortestLap = float('inf') #in milliseconds
        self.lastLapTimeStamp = float('inf')

class Boxxy(object):

    def __init__(self):
        #self.auth = OAuth(ACCESS_KEY, ACCESS_SECRET, CONSUMER_KEY, CONSUMER_SECRET)
        #self.twitter = Twitter(auth = auth)
        self.teams = {}

    def ping(self, json):
        print(json)

    def state(self, state):
        print(state)

        for teamid in state["teams"]:
            teamjson = state['teams'][teamid]
            self.teams[teamid] = Team(teamjson["name"])

    def lap(self, lap):
        print(lap)
        team = self.teams[lap["team"]]
        team.laps = lap["total"]
        totalLaps = lap["id"]

        #check teamtriggers
        '''laptime = lap["timestamp"] - team.lastLapTimeStamp
        if laptime < team.shortestLap and team.laps > 10:
            team.shortestLap = laptime
            #TRIGGER SHORTESTLAP
            #_ = twitter.statuses.update(status=msg)

        if team.laps % 100 == 0:
            pass
            #TRIGGER LAPMILESTONE
            #_ = twitter.statuses.update(status=msg)

        #check globaltriggers
        if laptime < shortestLapGlobal and totalLaps > 100:
            shortestLapGlobal = laptime
            #TRIGGER SHORTESTLAPGLOBAL
            #_ = twitter.statuses.update(status=msg)

        distance = totalLaps * OMTREK
        if distance > DISTANCES[0]:
            DISTANCES.popleft()
            #TRIGGER AFSTANDGELOPEN
            #_ = twitter.statuses.update(status=msg)

        team.lastLapTimeStamp = lap["timestamp"]'''

    def position(self, position):
        print(position)


def tweet(msg):
    pass
    #_ = twitter.statuses.update(status = msg)

main()
