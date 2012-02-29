# 12UrenLoop [![Build Status](https://secure.travis-ci.org/ZeusWPI/12Urenloop.png)](http://travis-ci.org/ZeusWPI/12Urenloop)

**TL;DR**: People run laps. We count, give them points and monitor it all.

This software is made for the yearly [12-Urenloop](http://12urenloop.be/) event. Small embedded computers with Bluetooth sensors running [Gyrid](https://github.com/Rulus/Gyrid) are placed around the circuit and forward information on detected Bluetooth devices (i.e. our batons).

## count-von-count

Count-von-count is the central component of the application. It analyzes incoming detection events and counts the rounds that were run, paying special attention to incomplete or irregular data. Teams automatically receive points for each round but can also receive points for special rounds or through other operator actions (Cheating? We'd never!).

All information and actions are permanently stored and can be replayed through the application. APIs to access this information are offered for other tools and score displays.

### Dependencies

*   [Haskell Platform](http://hackage.haskell.org/platform/)

    Run `cabal update` after installation to fetch the lastest package information, afterwards run `cabal install --only-dependencies` from the count-von-count folder to install all Haskell dependencies.

*   [mongoDB](http://www.mongodb.org/)

### Usage

To build the application run `cabal configure` and `cabal build` from the count-von-count folder. The executable is now available as `dist/build/count-von-count/count-von-count`.

Developing the application:

1. Run `ghci` from the project folder
2. Load the main source file: `:l src/CountVonCount/Main.hs`
3. Enter `main` to start the program, `^C` to interrupt
4. Enter `:r` to reload all changed modules

Testing the application:

1. Install the test framework, using `cabal install test-framework-hunit`
2. Execute `runghc -isrc -itests tests/CountVonCount/TestSuite.hs`

### API

Count-von-count offers the following methods through a JSON API.

*   `/config.json`
*   `/stream`

## Utilities

### pokemon

Demo application that generates detection events and sends them to a count-von-count process. Requires [LÖVE](https://love2d.org/) to run.

### tools/data-analysis

Checks the gathered data for inconsistencies.

### tools/monitor.rb

Runs a series of checks on each monitored host.

### tools/macalive.sh

Checks if a bluetooth device is still alive using the hcitool command. Used to assess the lifetime of our batons.
