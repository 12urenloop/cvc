#!/usr/bin/mongo

db.teams.drop();
db.laps.drop();

teams = [
    {"id": "t1", "name": "Bulbasaur",  "laps": 0, "baton": "20:11:02:15:01:80"},
    {"id": "t2", "name": "Machop",     "laps": 0, "baton": "20:11:02:15:01:42"},
    {"id": "t3", "name": "Mankey",     "laps": 0, "baton": "20:11:02:15:01:15"},
    {"id": "t4", "name": "Charmander", "laps": 0, "baton": "20:11:02:15:01:75"}
];

for(i in teams) db.teams.save(teams[i]);
