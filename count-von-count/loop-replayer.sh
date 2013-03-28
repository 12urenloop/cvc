#!/bin/sh
while [ 1 ]; do
    echo "Starting new iteration of count-von-count-replayer"

    pkill -u urenloop count-von-count
    cp count-von-count-fresh.db count-von-count.db

    nohup dist/build/count-von-count/count-von-count >> ~/log/count-von-count.log &
    sleep 1

    dist/build/count-von-count-replayer/count-von-count-replayer replay-2012.log
    sleep 5
done
