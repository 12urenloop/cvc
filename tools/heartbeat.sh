#!/bin/bash

SIHEMO="count:8002"
GROUP=$(hostname)
ALIVE="30"

# Arguments: phrase, state (0 for OK)
function sihemo {
  URI="$SIHEMO/services/$GROUP/$1"
  if [[ "$2" = 0 ]]; then
      wget -O - --post-data "alive=$ALIVE" "$URI" >/dev/null 2>&1
  else
      wget -O - --post-data "state=down" "$URI" >/dev/null 2>&1
  fi
}

while [[ true ]]; do
  BLUETOOTH=$(hcitool dev | grep -v Devices)
  test "$BLUETOOTH" != ""
  sihemo "Bluetooth" "$?"

  GYRID=$(pgrep gyrid)
  test "$GYRID" != ""
  sihemo "Gyrid" "$?"

  CONN=$(netstat -t -n | tail -n +3 | tr -s ' ' | cut -d' ' -f5 | grep 9001)
  test "$CONN" != ""
  sihemo "Connection to count-von-count" "$?"

  LOAD=$(cat /proc/loadavg | cut -d' ' -f1 | awk '{print(int($0 * 100))}')
  test "$LOAD" -lt 100
  sihemo "Load average OK" "$?"

  sleep 5
done
