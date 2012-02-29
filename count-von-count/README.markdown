# count-von-count

## Boxxy API

Most data calculated in count-von-count is forwarded to boxxy, which is a
front-end web server. All data is pushed by count-von-count to boxxy, boxxy
should never pull for information.

The rest of this section is a description of the different API calls which are
made to boxxy.

A few comments:

- All data sent is in JSON format (`Content-Type` set to `application/json`)
- A query string always includes a `key` param, e.g. `PUT /teams?key=tetten`

### Initialization

    PUT /teams

Body:

    [
      {
        "id": 1,
        "name": "Bulbasaur",
        "baton": "00:00:00:00:00:01",
        "laps": 0
      },
      ...
    ]

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
