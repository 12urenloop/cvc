#!/bin/python3
import sys
from datetime import datetime

ROLLING_AVG = 3



TEAMS = ["HILOK", "VTK", "VLK", "VGK", "VEK", "Hermes, Veto & Lila",
         "Wetenschappen & VLAK", "VPPK & Moeder Lies",
         "HK", "VRG", "Blandinia", "WVK", "SK", "Politea",
         "CHARPA", "VBK", "VDK & Pharma"]

team_len = max(map(len, TEAMS))

DIFFS = {name: [] for name in TEAMS}
PREVS = {name: datetime.now() for name in TEAMS}

now = datetime.now()
team_diff = []

for line in sys.stdin:
    str_split = line.strip().split("Lap for ")
    if len(str_split) == 2:
        team_name = str_split[1]
        timestamp = str_split[0].split()[0][1:-1]
        hour, minute, second = timestamp.split(':')
        current = datetime(now.year, now.month, now.day, int(hour), int(minute), int(second))
        diff = current - PREVS[team_name]
        team_diff = DIFFS[team_name]
        team_diff.append(diff.seconds)
        if len(team_diff) >= 2:
            total_avg = sum(team_diff[1:]) / (len(team_diff) - 1)
            rolling = team_diff[-3:]
            rolling_avg = sum(rolling)/len(rolling)
            print(f"{team_name.ljust(team_len)}: Cur.: {diff.seconds}s Rol.: {int(rolling_avg)}s Tot.: {int(total_avg)}s DIFF: {int(diff.seconds - rolling_avg)}s")
        PREVS[team_name] = current
