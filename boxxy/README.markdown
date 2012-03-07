![Boxxy](http://i.imgur.com/5Wet3.png)

### Description

This directory currently contains a small interface called Boxxy which
the counting software will send information too. The purpose of this interface
is to be the construction base for building visualisations.

TODO: we need a styleguide for Javascript

### Dependencies

* Node.js >= v6
* [Faye](http://faye.jcoglan.com/), install using `npm install faye`

### Running

From the server directory, run `node server.js`. Open the demo application
`demo.html` in your favorite webbrowser.

### Count Von Count - Boxxy Interface

Most data calculated in count-von-count is forwarded to boxxy, which is a
front-end web server. All data is pushed by count-von-count to boxxy, boxxy
should never pull for information.

The rest of this section is a description of the different API calls which are
made to boxxy.

A few comments:

- All data sent is in JSON format (`Content-Type` set to `application/json`)
- A query string always includes a `key` param, e.g. `PUT /teams?key=tetten`
- As for units, all positions are in meters, all times in seconds, and speed is
  expressed in meter/seconds

#### Initialization

    PUT /config

Body:

    {
      "stations": [
        {
          "position": 0.0,
          "name": "localhost",
          "mac": "00:00:00:00:01:00"
        },
        ...
      ],
      "circuitLength": 400.0,
      "teams": [
        {
          "id": 1,
          "name": "Bulbasaur",
          "baton": "00:00:00:00:00:01",
          "laps": 0
        },
        ...
      ]
    }

### Laps

    PUT /:teamid/laps

E.g. `PUT /1/laps`

Body:

    {
      "count": 1,
      "team": {
        "id": 1,
        "name": "Bulbasaur",
        "baton": "00:00:00:00:00:01",
        "laps": 0
      }
    }

### Position updates

    PUT /:teamid/position

E.g. `PUT /1/position`

Body:

    {
      "speed": 12.382769228025971,
      "station": {
        "position": 100.0,
        "name": "gyrid-2",
        "mac": "00:00:00:00:02:00"
      },
      "team": {
        "id": 1,
        "name": "Bulbasaur",
        "baton": "00:00:00:00:00:01",
        "laps": 0
      }
    }
