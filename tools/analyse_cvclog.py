
"""
Anylyse the CVC log to generate some plots. This should actually be
done with the database to be correct.

Argument: count-von-count.log
"""

import numpy as np
import matplotlib.pyplot as plt
import itertools
import sys
import collections

length = 380
start = 9 # o'clock
now = 60 * 60 * 12 # how many seconds after the start the given log was taken

def time_to_relative_secs(timestring):
    hours, minutes, seconds = timestring.split(':')
    return 60 * 60 * (int(hours) - start) + 60 * int(minutes) + int(seconds)

lapcounts = dict()

with open(sys.argv[1], 'r') as f:
    for line in f:

        line = line.strip()
        team = line.split(':')[-2][1:]
        if team not in lapcounts:
            # New team, let's make a new array for their points
            lapcounts[team] = []

        if "Lap for" in line:
            lapcounts[line.split('for ')[-1]].append(
                    time_to_relative_secs(line.split(']')[0][1:]))

# filtering non-teams
filteredcounts = dict()
for team, times in lapcounts.items():
    if times: filteredcounts[team] = times

# prediction
for team, times in filteredcounts.items():
    current = len(times)
    y = list(abs(current - now / t * i))
            for i, t in enumerate(times))
    plt.plot(times, y, label=team)
plt.show()
plt.clf()

# laps in function of time
for team, times in sorted(filteredcounts.items(), key=lambda tup: -len(tup[1])):
    times = np.array(times)
    plt.plot(times, range(len(times)), label="{} ({})".format(team, len(times)), linestyle='solid')
plt.legend(loc='upper left')
plt.xlabel('time in seconds')
plt.ylabel('number of laps')
plt.show()
plt.clf()

# laptime in function of time
for team, times in sorted(filteredcounts.items(), key=lambda tup: -len(tup[1])):
    if team != 'VBK': continue # too cluttered for all
    times = np.array(times)
    plt.scatter(times[1:], times[1:] - times[:-1], label=team)
plt.legend(loc='upper left')
plt.xlabel('time in seconds')
plt.ylabel('time per lap')
plt.show()
plt.clf()

