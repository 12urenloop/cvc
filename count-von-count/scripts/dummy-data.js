#!/usr/bin/mongo

db.teams.drop();
db.teams.save({"name": "Bulbasaur",  "laps": 0, "baton": "00:00:00:00:00:01"});
db.teams.save({"name": "Machop",     "laps": 0, "baton": "00:00:00:00:00:02"});
db.teams.save({"name": "Mankey",     "laps": 0, "baton": "00:00:00:00:00:03"});
db.teams.save({"name": "Charmander", "laps": 0, "baton": "00:00:00:00:00:04"});
