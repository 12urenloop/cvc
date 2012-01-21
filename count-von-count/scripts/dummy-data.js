#!/usr/bin/mongo count-von-count

db.teams.drop();
db.batons.drop();

for(var i = 0; i < 20; i++) {
    team = {"name": "Team " + i, "laps": 0};
    db.teams.save(team);
}

for(var i = 0; i < 30; i++) {
    baton = {"nr": i, "mac": "00:00:00:" + i};
    db.batons.save(baton);
}
