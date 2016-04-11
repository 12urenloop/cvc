from datetime import datetime, timedelta
from collections import deque
from random import choice
from os.path import isfile
import traceback
import sys

from socketIO_client import SocketIO, LoggingNamespace
from twitter import Twitter, OAuth
import jsonpickle as jp

from config import ACCESS_KEY, ACCESS_SECRET, CONSUMER_KEY, CONSUMER_SECRET

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
    (1903660, 'Sint-Petersburg', True),
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
    (3807320, 'Sint-Petersburg', False),
    (4357700, 'Istanbul', False),
    (4506520, 'Moskou', False),
])

TEAM_RUN_ROUNDS = ["{team} {verb} al {laps} rondjes achter de rug.",
                   "{team} {verb} al {laps} rondjes gelopen.",
                   "Nog eens honderd laps erbij! {team} {verb} al {laps} rondjes gelopen."]

TOTAL_DISTANCE = [
    "Alle teams tesamen hebben al de afstand van Gent tot {location}{andBack} overbrugt. Dat is niet minder dan {distance}!",
    "Blijven lopen: de lopers hebben al {distance} gelopen. Dat is ongeveer de afstand van Gent tot {location}{andBack}."]

FILENAME = 'save.json'

def main():
    global TWITTER
    boxxy = None
    if isfile(FILENAME):
        # save file so create boxxy from file
        with open(FILENAME, 'r') as f:
            boxxy = jp.decode(f.read())
    else:
        # no save so create new client
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

class Boxxy(object):
    def __init__(self):
        self.teams = {}
        self.omtrek = float("inf")
        self.distances = DISTANCES

    def ping(self, json):
        pass

    def state(self, state):
        print("new state")

        # set important vars
        self.omtrek = int(state["circuitLength"])

        for teamid in state["teams"]:
            teamjson = state['teams'][teamid]
            self.teams[teamid] = Team(teamjson["name"])

    def lap(self, lap):
        team = self.teams[lap["team"]]
        team.laps = lap["total"]
        totalLaps = int(lap["id"])

        if not lap["reason"] or len(lap["reason"]) > 0:
            print("Special lap")
            return

        if team.laps % 100 == 0:
            tweet(choice(TEAM_RUN_ROUNDS).format(team=team.name, laps=team.laps, verb=pluralize(team.name)))

        # check globaltriggers

        distance = totalLaps * self.omtrek
        if distance > self.distances[0][0]:
            loc = self.distances.popleft()
            # create msg
            andBack = ' (en terug)' if not loc[2] else ''
            tweet(
                choice(TOTAL_DISTANCE).format(location=loc[1], andBack=andBack, distance=convert_distance(distance)))

        self.dump()

    def position(self, position):
        pass

    def dump(self):
        with open(FILENAME, 'w') as f:
            print(jp.encode(self), end='', file=f)


def parse_time(time):
    return datetime.strptime(time, "%Y-%m-%dT%H:%M:%S.%fZ")


def pluralize(teamname):
    if '&' in teamname or teamname[-1] is 's':
        return "hebben"
    return "heeft"

def convert_distance(distance):
    return str(int(distance/1000)) + "km"


auth = OAuth(ACCESS_KEY, ACCESS_SECRET, CONSUMER_KEY, CONSUMER_SECRET)
twitter = Twitter(auth=auth)

def tweet(msg):
    print("Tweet: " + msg + " (" + str(len(msg)) + ")")
    try:
        _ = twitter.statuses.update(status=msg)
    except:
        print("Twitter error")
        traceback.print_exc(file=sys.stdout)

main()
