#!/bin/bash
set -o nounset -o errexit -o pipefail

if [ ! -e 'count-von-count.cabal' ]; then
    echo 'Run this from the count-von-count directory!'
    exit 1
fi

while [ 1 ]; do
    echo "[$(date)] Starting new iteration of count-von-count-replayer"

    pkill -u urenloop count-von-count
    sleep 1
    runghc -isrc 'scripts/teams2012.hs'

    nohup dist/build/count-von-count/count-von-count >> ~/log/count-von-count.log &
    sleep 1

    dist/build/count-von-count-replayer/count-von-count-replayer replay-2012.log
    sleep 5
done
