

from functools import wraps
from flask import Flask, request, Response
from collections import deque

app = Flask(__name__)

DISTANCES = deque([
    #(distance,name),
    
    ])

def main():
    boxxy = Boxxy()
    paths = {
        "/ping": boxxy.ping,
        "/state": boxxy.state,
        "/lap": boxxy.lap,
        "/position": boxxy.position
        }
    for path, method in paths.items():
        app.route(path, methods=['PUT'])(requires_auth(method))
    app.run()

class Team:

    def __init__(self,name):
        self.name = name
        self.shortestLap = sys.maxint #in milliseconds
        self.lastLapTimeStamp

class Boxxy(object):

    teams = []

    def ping(self):
        print(request.json)
        return "OK"

    def state(self):
        print(request.json)
        state = request.json

        for teamjson in state["teams"]:
            teams[teamjson["id"]] = Team(teamjson["name"])

        return "OK"

    def lap(self):
        print(request.json)
        lap = request.json

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

        return "OK"

    def position(self):
        print(request.json)
        return "OK"


def check_auth(username, password):
    """This function is called to check if a username /
    password combination is valid.
    """
    return username == 'count-von-count' and password == 'tetten'

def authenticate():
    """Sends a 401 response that enables basic auth"""
    return Response(
    'Could not verify your access level for that URL.\n'
    'You have to login with proper credentials', 401,
    {'WWW-Authenticate': 'Basic realm="Login Required"'})

def requires_auth(f):
    @wraps(f)
    def decorated(*args, **kwargs):
        auth = request.authorization
        if not auth or not check_auth(auth.username, auth.password):
            return authenticate()
        return f(*args, **kwargs)
    return decorated

def tweet(msg):
    pass

