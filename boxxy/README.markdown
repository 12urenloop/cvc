![Boxxy](http://i.imgur.com/5Wet3.png)

Boxxy
=====

Boxxy is the application which powers <http://live.12urenloop.be>. It
functionality can be divided into two big parts:

- Receive info and updates from `count-von-count`
- Serve this information, together with visualisations to the users

Installing & running
====================

Install all dependencies using

    npm install

Run the server with

    node src/server.js

JavaScript API
==============

Boxxy provides a JavaScript API which facilitates writing visualisations.

Initialization
--------------

If the boxxy module is in scope, the initialize call provides you with an object
that has all further information and methods:

    var boxxy = boxxy.initialize();

Accessible fields
-----------------

The `boxxy` object has a number of fields. These should be used in a read-only
way.

- `boxxy.circuitLength` is simply the length of the running track (in meters).

- `boxxy.startTime` is a string containing the date the event started.

- `boxxy.stations` holds an object with the *stations*. The stations are simply
  the bluetooth receivers positioned around the circuit.

        {
            1: {
                name: "station 1",
                id: "1",
                position: 0
            },
            ...
        }

    `position` is the position of the station along the circuit (in meters).

- `boxxy.teams` holds an object with all the teams and their current scores.

        {
            1:{
                name: "Politeia",
                updated: "2013-03-22T22:22:59.971Z",
                id: "1",
                laps: 23,
                station: "3"
            },
            ...
        }

    `station` refers to the ID of the station where the team was last seen. This
    means you can do something like:

        boxxy.stations[boxxy.teams["1"].station]

- `boxxy.laps` holds an array with the `n` latest laps. This `n` can be
  configured by changing the variable `boxxy.maxLaps`.

        [
            {
                timestamp: "2013-03-22T22:30:23.867Z",
                team:"4",
                total: 28,
                id: "113",
                reason: null,
                count: 1
            },
            {
                timestamp: "2013-03-22T22:27:12.489Z",
                team: "3",
                total: 26,
                id: "104",
                reason: "Draag de Praeses bonusronde",
                count: 3
            },
            ...
        ]

    `total` is the total amount of laps up to that point. `count` is the number
    of points assigned for the lap. This is usually one, except for bonus laps:
    in the case of the latter, `reason` can also be provided by boxxy.

Callbacks
---------

The server will send .messages to the client using the Socket.IO library. There
is no need to manually deal with these messages. Instead, the user should set
callbacks in the boxxy object. All callbacks are optional (but you probably want
to set at least one).

- `boxxy.onPutState = function(stateDelta) {}`

    This function is called when the boxxy object receives the initial list of
    teams, stations and laps. This method is also called when there has been a
    connection interrupt. The `stateDelta` argument is best ignored in client
    code, as you can use `boxxy` instead.

- `boxxy.onAddLap = function(lap) {}`

    This function is called whenever a team completes a lap. The `lap` argument
    has more information regarding the lap.

- `boxxy.onUpdatePosition = function(position) {}`

    When the position of a team changes, this method is called. The `position`
    argument is an object that looks like this:

        {
            timestamp: "2013-03-22T22:36:59.670Z",
            station: "4",
            team: "4"
        }

- `boxxy.onUpdate = function() {}`

    When any of the above callbacks are invocated, the `onUpdate` function is
    also called. This is useful if you want to the same thing in every callback:
    just set `onUpdate` and leave the other callbacks unset.

Functions
---------

- `boxxy.listen(uri)` conects the client object to the server. You should call
  this after setting up all your callbacks.

- `boxxy.teamsByScore()` returns an array with all the teams from `boxxy.teams`,
  ordered by score (total number of laps).

HTTP API
========

Freezing and setting notifications
----------------------------------

`boxxy` allows displaying a notification to the users. This can be done using:

    curl -XPUT localhost:8080/state \
        -H 'Content-Type: application/json' \
        -u 'count-von-count:tetten' \
        -d '{"notification": "This is a message"}'

At some point close to the end of the competition, we may want to freeze the
displayed scores. This can be done using:

    curl -XPUT localhost:8080/state \
        -H 'Content-Type: application/json' \
        -u 'count-von-count:tetten' \
        -d '{"frozen": true}'

Unfreezing can be done by setting the same parameter to `false`.
