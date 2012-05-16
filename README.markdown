# 12UrenLoop [![Build Status](https://secure.travis-ci.org/ZeusWPI/12Urenloop.png)](http://travis-ci.org/ZeusWPI/12Urenloop)

**TL;DR**: People run laps. We count, give them points and monitor it all.

This software is made for the yearly [12-Urenloop](http://12urenloop.be/) event. Small embedded computers with Bluetooth sensors running [Gyrid](https://github.com/Rulus/Gyrid) are placed around the circuit and forward information on detected Bluetooth devices (i.e. our batons).

## count-von-count

Count-von-count is the central component of the application. It analyzes incoming detection events and counts the rounds that were run, paying special attention to incomplete or irregular data. Teams automatically receive points for each round but can also receive points for special rounds or through other operator actions (Cheating? We'd never!).

All information and actions are permanently stored and can be replayed through the application. APIs to access this information are offered for other tools and score displays.

## Utilities

### pokemon

Demo application that generates detection events and sends them to a count-von-count process.

To run, install [LÖVE](https://love2d.org/) and run `love` from the pokemon-directory.

### tools/data-analysis

Checks the gathered data for inconsistencies.

### tools/monitor.rb

Runs a series of checks on each monitored host.

### tools/macalive.sh

Checks if a bluetooth device is still alive using the hcitool command. Used to assess the lifetime of our batons.
