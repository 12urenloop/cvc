#!/usr/bin/mongo

db.teams.drop();
db.batons.drop();

for(var i = 0; i < 5; i++) {
    team = {"name": "Team " + i, "laps": 0, "baton": null};
    db.teams.save(team);
}

for(var i = 0; i < 10; i++) {
    baton = {"nr": i, "mac": "00:00:00:" + i, "team": null};
    db.batons.save(baton);
}
