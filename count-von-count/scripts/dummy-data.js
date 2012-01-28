#!/usr/bin/mongo

db.teams.drop();
db.laps.drop();

teams = [
    {"id": 1, "name": "Bulbasaur",  "laps": 0, "baton": "00:00:00:00:00:01"},
    {"id": 2, "name": "Machop",     "laps": 0, "baton": "00:00:00:00:00:02"},
    {"id": 3, "name": "Mankey",     "laps": 0, "baton": "00:00:00:00:00:03"},
    {"id": 4, "name": "Charmander", "laps": 0, "baton": "00:00:00:00:00:04"}
];

for(i in teams) db.teams.save(teams[i]);
