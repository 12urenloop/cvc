#!/usr/bin/mongo

db.teams.drop();

teams = [
    {'id': 't1',  'name': 'HomeKonvent',          'laps': 0, 'baton': null},
    {'id': 't2',  'name': 'SeniorenKonvent',      'laps': 0, 'baton': null},
    {'id': 't3',  'name': 'Kofschipclubs',        'laps': 0, 'baton': null},
    {'id': 't4',  'name': 'VEK',                  'laps': 0, 'baton': null},
    {'id': 't5',  'name': 'VTK',                  'laps': 0, 'baton': null},
    {'id': 't6',  'name': 'VLK',                  'laps': 0, 'baton': null},
    {'id': 't7',  'name': 'Blandinia',            'laps': 0, 'baton': null},
    {'id': 't8',  'name': 'Politeia',             'laps': 0, 'baton': null},
    {'id': 't9',  'name': 'VRG',                  'laps': 0, 'baton': null},
    {'id': 't10', 'name': 'Wetenschappen & Vlak', 'laps': 0, 'baton': null},
    {'id': 't11', 'name': 'VPPK',                 'laps': 0, 'baton': null},
    {'id': 't12', 'name': 'VGK & GFK & VBK',      'laps': 0, 'baton': null},
    {'id': 't13', 'name': 'KVHV',                 'laps': 0, 'baton': null},
    {'id': 't14', 'name': 'HILOK',                'laps': 0, 'baton': null}
];

for(i in teams) db.teams.save(teams[i]);
