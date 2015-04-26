from socketIO_client import SocketIO, LoggingNamespace
from functools import wraps
from datetime import datetime, timedelta
from collections import deque
from random import choice
from twitter import Twitter, OAuth

from config import *

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

SHORTEST_LAP = ["{team} {verb} net een rondje in {time} gelopen. Dat is hun snelste rondje tot nog toe!",
                "{team} {verb} hun persoonlijk record verbeterd: hun laatste rondje duurde slechts {time}. Doe zo voort!"]

GLOBAL_FASTEST_LAP = [
    "{team} {verb} net een rondje in {time} gelopen. Dat is tot nog toe het snelste rondje van de dag! Proficiat!",
    "De titel Snelste Rondje Van De Dag gaat momenteel naar {team}. Zij hebben hun laatste rondje in {time} gelopen! Proficiat!",
    "Een nieuw snelheidsrecord: {team} {verb} hun laatste rondje in {time} gelopen. Dat is Tot nu toe het snelste rondje van de dag! Proficiat!"]

TEAM_RUN_ROUNDS = ["{team} {verb} al {laps} laps achter de rug.",
                   "{team} {verb} al {laps} laps gelopen.",
                   "Nog eens honderd laps erbij! {team} {verb} al {laps} laps gelopen."]

TOTAL_DISTANCE = [
    "Alle teams tesamen hebben al de afstand van Gent tot {location}{andBack} overbrugt. Dat is niet minder dan {distance}!"
    "Blijven lopen: de lopers hebben al {distance} gelopen. Dat is zo ongeveer de afstand van Gent tot {location}{andBack}."]


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

    socketIO.wait(seconds=3600 * 12)


class Team:
    def __init__(self, name):
        self.name = name
        self.shortestLap = timedelta(minutes=12)
        self.lastLapTimeStamp = datetime.utcnow() - timedelta(hours=1)  # be certain


class Boxxy(object):
    def __init__(self):
        self.auth = OAuth(ACCESS_KEY, ACCESS_SECRET, CONSUMER_KEY, CONSUMER_SECRET)
        self.twitter = Twitter(auth=self.auth)
        self.teams = {}
        self.shortestLapGlobal = None
        self.omtrek = float("inf")

    def ping(self, json):
        pass

    def state(self, state):
        print("new state")

        # set important vars
        self.omtrek = int(state["circuitLength"])
        if self.shortestLapGlobal is None:
            self.shortestLapGlobal = timedelta(minutes=12)

        for teamid in state["teams"]:
            teamjson = state['teams'][teamid]
            self.teams[teamid] = Team(teamjson["name"])

    def lap(self, lap):
        team = self.teams[lap["team"]]
        team.laps = lap["total"]
        totalLaps = int(lap["id"])

        # check
        lapEndTimeStamp = parse_time(lap["timestamp"])
        laptime = lapEndTimeStamp - team.lastLapTimeStamp
        print(laptime)
        if laptime < team.shortestLap:
            team.shortestLap = laptime
            # TRIGGER SHORTESTLAP
            print("shortest lap ", laptime)
            if team.laps > 10:
                msg = choice(SHORTEST_LAP)
                self.tweet(msg.format(team=team.name, time=str(laptime)[3:-3], verb=pluralize(team.name)))

        if team.laps % 100 == 0:
            print("%d laps" % team.laps)
            self.tweet(choice(TEAM_RUN_ROUNDS).format(team=team.name, laps=team.laps, verb=pluralize(team.name)))

        # check globaltriggers
        if laptime < self.shortestLapGlobal:
            print("shortest global time", laptime)
            self.shortestLapGlobal = laptime
            if totalLaps > 100:
                msg = choice(GLOBAL_FASTEST_LAP)
                self.tweet(msg.format(team=team.name, time=str(laptime)[3:-3], verb=pluralize(team.name)))

        distance = totalLaps * self.omtrek
        if distance > DISTANCES[0][0]:
            loc = DISTANCES.popleft()
            print("distance %d" % distance)
            # create msg
            andBack = ' (en terug)' if loc[2] else ''
            self.tweet(
                choice(TOTAL_DISTANCE).format(location=loc[1], andBack=andBack, distance=str(loc[0] / 1000) + "km"))

        team.lastLapTimeStamp = lapEndTimeStamp

    def position(self, position):
        pass

    def tweet(self, msg):
        try:
            _ = self.twitter.statuses.update(status=msg)
        except:
            print("Twitter error")


def parse_time(time):
    return datetime.strptime(time, "%Y-%m-%dT%H:%M:%S.%fZ")

def pluralize(teamname):
    if '&' in teamname or teamname[-1] is 's':
        return "hebben"
    return "heeft"

main()
