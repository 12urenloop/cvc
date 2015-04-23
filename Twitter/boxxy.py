

from functools import wraps
from flask import Flask, request, Response
app = Flask(__name__)

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

class Boxxy(object):

    def ping(self):
        print(request.json)
        return "OK"

    def state(self):
        print(request.json)
        return "OK"

    def lap(self):
        print(request.json)
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

