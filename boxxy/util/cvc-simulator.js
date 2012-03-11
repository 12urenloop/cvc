var runner = require('./runner'),
    team = require('./team')
    http = require('http')

var names = ["Michiel", "Sander", "Pieter"]
var runners = [];
var config = {
    stations: [
        {
            position: 0.0,
            name: 'gyrid-1',
            mac: "00:00:00:00:01:00" 
        },
        {
            position: 100.0,
            name: 'gyrid-2',
            mac: "00:00:00:00:02:00" 
        },
        {
            position: 200.0,
            name: 'gyrid-3',
            mac: "00:00:00:00:03:00" 
        },
        {
            position: 300.0,
            name: 'gyrid-4',
            mac: "00:00:00:00:04:00" 
        }
    ],
    teams: [
        {
            id: 'team-1',
            name: 'Michiel',
            baton: "00:00:00:00:00:01",
            laps: 0
        },
        {
            id: 'team-2',
            name: 'Pieter',
            baton: "00:00:00:00:00:02",
            laps: 0
        },
        {
            id: 'team-3',
            name: 'Sander',
            baton: "00:00:00:00:00:03",
            laps: 0
        },
        {
            id: 'team-4',
            name: 'Jasper',
            baton: "00:00:00:00:00:04",
            laps: 0
        }
    ],
    circuitLength: 400.0
}

for(var idx in config.teams) {
    var t = new team.Team(config.teams[idx].id, config.teams[idx].name, config)
    
    t.on('position', function(message) {
        console.log(message)
        toBoxxy('/' + message.team.id + '/position', message)
    })
    t.on('laps', function(message) {
        console.log(message)
        toBoxxy('/' + message.team.id + '/laps', message)
    })
    t.run()
}

toBoxxy('/config', config)

function toBoxxy(path, message) {
    var options = {
        host: '127.0.0.1',
        port: 8080,
        path: path + "?key=tetten",
        method: 'PUT',
        headers: {
            'Content-Type': 'application/json'
        }
    };

    var req = http.request(options, function(res) {
        // console.log(res)
    });

    req.on('error', function(e) {
        console.log('problem with request: ' + e.message);
    });

    // write data to request body
    req.write(JSON.stringify(message));
    req.end();
}