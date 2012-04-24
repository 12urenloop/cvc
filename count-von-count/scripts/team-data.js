#!/usr/bin/mongo

db.teams.drop();

function addTeam(id, name) {
    db.teams.save({
        'id': id,
        'name': name,
        'laps': 0,
        'laps_': [],
        'baton': null
    });
}

addTeam('t1', 'HomeKonvent');
addTeam('t2', 'SeniorenKonvent');
addTeam('t3', 'Kofschipclubs');
addTeam('t4', 'VEK');
addTeam('t5', 'VTK');
addTeam('t6', 'VLK');
addTeam('t7', 'Blandinia');
addTeam('t8', 'Politeia');
addTeam('t9', 'VRG');
addTeam('t10','Wetenschappen & Vlak');
addTeam('t11','VPPK');
addTeam('t12','VGK & GFK & VBK');
addTeam('t13','KVHV');
addTeam('t14','HILOK');
