#!/usr/bin/mongo

db.teams.drop();

for(var i = 0; i < 5; i++) {
    team = {"name": "Team " + i, "laps": 0, "baton": null};
    db.teams.save(team);
}
