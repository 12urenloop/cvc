#!/bin/bash

SIHEMO="eva:8001"
GROUP=$(hostname)
ALIVE="30"

function heartbeat {
  wget -O - --post-data "alive=$ALIVE" "$SIHEMO/services/$GROUP/$1" >/dev/null 2>&1
}

function down {
  wget -O - --post-data "state=down" "$SIHEMO/services/$GROUP/$1" >/dev/null 2>&1
}

while [[ true ]]; do
  BLUETOOTH=$(hcitool dev | grep -v Devices)
  if [[ "$BLUETOOTH" = "" ]]; then
    down "Bluetooth"
  else
    heartbeat "Bluetooth"
  fi

  GYRID=$(pgrep gyrid)
  if [[ "$GYRID" = "" ]]; then
    down "Gyrid"
  else
    heartbeat "Gyrid"
  fi

  CONN=$(netstat -t -n | tail -n +3 | tr -s ' ' | cut -d' ' -f5 | grep 9001)
  if [[ "$CONN" = "" ]]; then
    down "Connection to count-von-count"
  else
    heartbeat "Connection to count-von-count"
  fi

  LOAD=$(cat /proc/loadavg | cut -d' ' -f1 | awk '{print(int($0 * 100))}')
  if [[ "$LOAD" -ge 100 ]]; then
    down "Load average OK"
  else
    heartbeat "Load average OK"
  fi

  sleep 5
done
