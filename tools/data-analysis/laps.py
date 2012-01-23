__author__ = 'Michiel Van den Berghe'
import statfunc
import time

import api

class History:
    warningDeviation = 0
    def __init__(self):
        self.lastUpdate = 0
        self.histories = {}

    def updateHistory(self, timestamp, data):
        self.lastUpdate = timestamp
        for stick, history in data.items():
            if stick not in self.histories:
                self.histories[stick] = StickHistory(stick)
            self.histories[stick].updateHistory(timestamp, history)

    def getWarnings(self):
        warnings = {}
        for stick, history in self.histories.items():
            dev, expected, stddev, lastLap = history.getDeviation()
            if dev > self.warningDeviation:
                warnings[stick] = (dev, expected, stddev, lastLap)
        return warnings

    def lastUpdated(self):
        return self.lastUpdate

class StickHistory:
    historySize = 10
    def __init__(self, stick):
        self.stick = stick
        self.history = []
        self.lastUpdate = 0

    def updateHistory(self, timestamp, laps):
        self.lastUpdate = timestamp
        self.history.extend(laps)
        self.history = self.history[-self.historySize:]

    def getLastLap(self):
        return self.history[-1]

    def getLapTimes(self):
        return statfunc.diff(self.history)

    def getDeviation(self):
        laptimes = self.getLapTimes()
        stdev = statfunc.stdev(laptimes)
        avgLap = statfunc.avg(laptimes)
        lastLap = self.getLastLap()
        # deviation, expected, stddev, lastLap
        return (self.lastUpdate - lastLap - avgLap) / stdev, lastLap + avgLap, stdev, lastLap


class HistoryUpdater:
    def __init__(self, history, fetcher, interval):
        self.history = history
        self.fetcher = fetcher
        self.interval = interval
        self.running = True

    def start(self):
        print("Statchecker running...")
        while self.running:
            print("Updating %s..." % time.ctime())
            timestamp, data = self.fetcher.getHistory(self.history.lastUpdated())
            time.sleep(self.interval)
            self.history.updateHistory(timestamp, data)
            for team, info in self.history.getWarnings().items():
                dev, expected, stddev, lastLap = info
                print("Warning: %s is %f standaardafwijkingen (%f) te laat. Laatste lap: %s" % (team, dev, stddev, time.ctime(lastLap)))
            print("Updated %s" % time.ctime(timestamp))

    def stop(self):
        self.running = False


if __name__ == '__main__':
   updater = HistoryUpdater(History(), api.HistoryFetcher(api.url), 10)
   updater.start()