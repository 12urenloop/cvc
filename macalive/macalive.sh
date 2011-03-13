#!/bin/bash

echo "Checking how long $1 stays alive. Interval: $2, allowed to miss $3 ticks."

STARTTIME=`date`
LASTSEEN='NOT YET'
MISSEDTICKS=0

while [ $MISSEDTICKS -lt $3 ]; do
	hcitool inq | awk -v mac="$1" -f contains.awk

	if [ $? -eq 0 ]; then
		echo "$1 is still alive"
		LASTSEEN=`date`
		let 'MISSEDTICKS = 0'
	else
		let "MISSEDTICKS += 1"
		echo "$1 missed $MISSEDTICKS tick(s)"
	fi

	sleep $2
done

echo "$1 missed $3 ticks yo, nigga dead. RIP."
echo "Started at $STARTTIME and last seen at $LASTSEEN"
