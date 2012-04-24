#!/bin/bash
#  _____   _   _ U _____ u       ____      _      __  __  U _____ u
# |_ " _| |'| |'|\| ___"|/    U /"___|uU  /"\  uU|' \/ '|u\| ___"|/
#   | |  /| |_| |\|  _|"      \| |  _ / \/ _ \/ \| |\/| |/ |  _|"
#  /| |\ U|  _  |u| |___       | |_| |  / ___ \  | |  | |  | |___
# u |_|U  |_| |_| |_____|       \____| /_/   \_\ |_|  |_|  |_____|
# _// \\_ //   \\ <<   >>       _)(|_   \\    >><<,-,,-.   <<   >>
#(__) (__)_") ("_)__) (__)     (__)__) (__)  (__)(./  \.) (__) (__)

if [ $# -lt 2 ]; then
	echo "Usage: ./macalive.sh <mac addy> <amount of ticks allowed to miss>"
	exit 42
fi

echo "Checking how long $1 stays alive. Allowed to miss $2 inquiries."

STARTTIME=`date`
LASTSEEN=$STARTTIME
MISSEDTICKS=0

while [ $MISSEDTICKS -lt $2 ]; do
	hcitool inq --flush | grep "$1"

	if [ $? -eq 0 ]; then
		echo "[" `date` "] $1 is still alive"
		LASTSEEN=`date`
		let 'MISSEDTICKS = 0'
	else
		let "MISSEDTICKS += 1"
		echo "[" `date` "] $1 missed $MISSEDTICKS tick(s)"
	fi
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
