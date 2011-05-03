count-von-count
===============

Backend. Receives bluetooth measurements from another component, and analyzes
the data in order to count laps.

Installing & running
--------------------

Install [cabal-install](http://www.haskell.org/cabal/), the easiest way is to
use your platform's package manager.

Compile `count-von-count`:

    cabal configure
    cabal build

Installation (optional):

    cabal install

Installation is not strictly necessary, you can also run it as

    ./dist/build/count-von-count/count-von-count

By default, it assumes the config file is called `config.yaml` and located in
the current directory. You can override the default by passing another file as
argument:

    ./dist/build/count-von-count/count-von-count myconf.yaml

Run the tests:

    runghc -isrc -itests tests/TestSuite.hs

Features
--------

### Gyrid

`count-von-count` is designed to work together with Gyrid. It listens for Gyrid
input on a port specified in the configuration file.

### Replay logs

An extension to the Gyrid format is made:

    REPLAY,timestamp,<normal RSSI data>

is interpreted as regular RSSI data from Gyrid, but it uses the given timestamp
instead of the server timestamp. This allows you to quickly replay a log:

    cp log.csv replay.csv
    netcat localhost 9001 < replay.csv

### Admin interface

`count-von-count` provides a web admin interface. It can be accessed on the port
specified in the configuration file. This web interface allows the admin to see
the live data and optionally reset certain counters.

### RSSI treshold

`count-von-count` can ignore Gyrid input when the RSSI value is too low. To use
this, set the `RSSI treshold` value in `config.yaml`. When you want to receive
all data, set it to `0`.
