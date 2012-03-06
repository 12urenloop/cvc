var runner = require('./runner'),
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
    teams: []
}

for(var idx in names) {
    var r = new runner.Runner(parseInt(idx), names[idx])
    config.teams.push({
        id: r.id,
        name: r.name,
        baton: "00:00:00:00:00:0" + r.id,
        laps: 0
    })
    r.on('position', function(message) {
        console.log(message)
        toBoxxy('/' + r.id + '/position', message)
    })
    r.on('laps', function(message) {
        console.log(message)
        toBoxxy('/' + r.id + '/laps', message)
    })
    r.start()
    runners.push(runner)
}

toBoxxy('/config', config)

function toBoxxy(path, message) {
    var options = {
        host: 'localhost',
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