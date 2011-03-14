#!/bin/bash

echo "Checking how long $1 stays alive. Allowed to miss $2 inquiries."

STARTTIME=`date`
LASTSEEN=$STARTTIME
MISSEDTICKS=0

while [ $MISSEDTICKS -lt $2 ]; do
	hcitool inq | awk -v mac="$1" -f contains.awk

	if [ $? -eq 0 ]; then
		echo "$1 is still alive"
		LASTSEEN=`date`
		let 'MISSEDTICKS = 0'
	else
		let "MISSEDTICKS += 1"
		echo "$1 missed $MISSEDTICKS tick(s)"
	fi
	sleep 10 # Sleep this long because hcitool doesnt refresh for another 10 seconds
done

echo
echo "Yo, $1 missed $2 ticks, nigga dead. RIP."
echo "Started at $STARTTIME and last seen at $LASTSEEN"

STARTSEC=`date +%s -d "$STARTTIME"`
STOPSEC=`date +%s -d "$LASTSEEN"`
((ALIVETIME=STOPSEC-STARTSEC))
((H=ALIVETIME/3600))
((M=ALIVETIME%3600/60))
((S=ALIVETIME%60))

echo "Total alive time: $H:$M:$S"
