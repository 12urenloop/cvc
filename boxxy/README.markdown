# Boxxy

## Beschrijving
Dit is een kleine proof of concept voor een Boxxy gebaseerd op Node en Faye.
Vier lopers doen rondjes, een voorbeeldvisualisatie geeft aan wanneer ze een rondje voltooien en wat hun gemiddelde snelheid voor het rondje was.

## Dependencies
* Node.js >= v6
* Een moderne versie van Faye (http://faye.jcoglan.com/)
  Faye is eenvoudig te installeren mbv "npm install faye"

## Runnen
De server: "node fayeserver.js" in de server directory
De visualisatie: client/index.html openen in jouw favoriete browser


## Client Debug
If you want to connect the example client to a server different from live.12urenloop.be, add a config.js file
in the same directory containing

    var config = {
        host: <server>,
        port: <port>
    }

## Count Von Count - Boxxy Interface

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

### Initialization

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

E.g.

    PUT /1/laps

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

E.g.

    PUT /1/position

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
