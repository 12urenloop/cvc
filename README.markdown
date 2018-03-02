# 12urenloop [![Build Status](https://travis-ci.org/ZeusWPI/12urenloop.svg?branch=master)](https://travis-ci.org/ZeusWPI/12urenloop) [![Coverage Status](https://coveralls.io/repos/ZeusWPI/12urenloop/badge.svg?branch=master)](https://coveralls.io/r/ZeusWPI/12urenloop?branch=master) [![Analytics](https://ga-beacon.appspot.com/UA-25444917-6/ZeusWPI/12Urenloop/README.markdown?pixel)](https://github.com/igrigorik/ga-beacon)

**TL;DR**: People run laps. We count, give them points and monitor it all.

This software is made for the yearly [12urenloop](https://12urenloop.be/) event. Small embedded computers with Bluetooth sensors running [Gyrid](https://github.com/Roel/Gyrid) are placed around the circuit and forward information on detected Bluetooth devices (i.e. our batons).

## count-von-count

Count-von-count is the central component of the application. It analyzes incoming detection events and counts the rounds that were run, paying special attention to incomplete or irregular data. Teams automatically receive points for each round but can also receive points for special rounds or through other operator actions (Cheating? We'd never!).

All information and actions are permanently stored and can be replayed through the application. APIs to access this information are offered for other tools and score displays.

[More information](count-von-count/README.markdown)

## boxxy

Boxxy is an application-level proxy server which receives information from count-von-count and distributes it again to a larger number of clients through websockets.

[More information](boxxy/README.markdown)

## Utilities and other programs

### pokemon

Demo application that generates detection events and sends them to a count-von-count process.

To run, install [LÖVE](https://love2d.org/) and run `love .` from the pokemon-directory or `love pokemon` from the repository root.

### manual-count

Small web application that serves as a backup counting system.

### `tools/data-analysis`

Checks the gathered data for inconsistencies.

### `tools/dj-ratings.rb`

Counts the SMS-votes for the DJ-contest.

### `tools/heartbeat.sh`

Runs a series of checks on each monitored host and sends a heartbeat signal to [sihemo](https://github.com/jaspervdj/sihemo).

### `tools/macalive.sh`

Checks if a bluetooth device is still alive using the hcitool command. Used to assess the lifetime of our batons.
