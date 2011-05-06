__author__ = 'Michiel Van den Berghe'

import urllib.request
import json
import time

def timestamp(date):
    if '.' in date:
        date = date.split('.')[0]
    else:
        date = date.split('+')[0]
    return int(time.mktime(time.strptime(date,'%Y-%m-%dT%H:%M:%S')))
        
url = 'http://10.0.0.206/dr.beaker/api/history'
class HistoryFetcher:
    def __init__(self, url):
        self.url = url

    def getHistory(self, since):
        ret = {}
        url = self.url + '?since=%d' % since
        page = urllib.request.urlopen(url)
        data = json.loads(page.read().decode())
        if type(data['historyEntry']) != list:
            last = timestamp(data['historyEntry']['date'])
        else:
            last = max([timestamp(entry['date']) for entry in data['historyEntry']])
        for entry in data['historyEntry'] if type(data['historyEntry']) == list else [data['historyEntry']]:
            name = entry['team']['name']
            if name not in ret:
                ret[name] = []
            ret[name].append(timestamp(entry['date']))
        for team in ret:
            ret[team].sort()
        return last, ret

if __name__ == '__main__':
    h = HistoryFetcher(url)
    date, data = h.getHistory(0)
    print(date, data)