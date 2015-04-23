import logging
logging.getLogger('requests').setLevel(logging.WARNING)
logging.basicConfig(level=logging.DEBUG)

from socketIO_client import SocketIO, LoggingNamespace

def on_aaa_response(*args):
    print('on_aaa_response', args)

socketIO = SocketIO('live.12urenloop.be', 8080)
#socketIO.on('/state', on_aaa_response)
#socketIO.emit('')
#socketIO.wait(seconds=10)