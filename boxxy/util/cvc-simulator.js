var team = require('./team')
    http = require('http')

var names = ["Michiel", "Sander", "Pieter"]
var runners = [];
var names = [
    "HomeKonvent", "SeniorenKonvent", "Kofschipclubs", "VEK", "VTK", "VLK", 
    "Blandinia", "Politeia", "VRG", "Wetenschappen & VLAK", "VPPK",
    "VGK & GFK & VBK", "KVHV", "HILOK"
];
var config = {
    startTime: team.ISODateString(new Date()),
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
    teams: [],
    circuitLength: 400.0
}

for(var idx in names) {
    config.teams.push({
        id: 'team-' + idx,
        name: names[idx],
        baton: "00:00:00:00:00:" + (idx < 10 ? "0" + idx : idx),
        laps: 0
    })
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
    })

    req.on('error', function(e) {
        console.log('problem with request: ' + e.message)
    })

    // write data to request body
    message.time = team.ISODateString(new Date())
    req.write(JSON.stringify(message))
    req.end()
}