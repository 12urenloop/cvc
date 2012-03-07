#!/usr/bin/mongo

db.teams.drop();
db.laps.drop();

teams = [
    {"id": "t1", "name": "Bulbasaur",  "laps": 0, "baton": "00:00:00:00:00:01"},
    {"id": "t2", "name": "Machop",     "laps": 0, "baton": "00:00:00:00:00:02"},
    {"id": "t3", "name": "Mankey",     "laps": 0, "baton": "00:00:00:00:00:03"},
    {"id": "t4", "name": "Charmander", "laps": 0, "baton": "00:00:00:00:00:04"}
];

for(i in teams) db.teams.save(teams[i]);
