var express = require('express'),
    faye = require('faye'),

    auth = require('./auth')

run = function(port) {
    var server = express.createServer(),
        bayeux = new faye.NodeAdapter({mount: '/boxxy', timeout: 45})
    bayeux.attach(server)

    // Configure the faye server to use the correct keys
    bayeux.addExtension(auth.serverAuth)
    bayeux.getClient().addExtension(auth.clientAuth)

    // Parse messages as JSON
    server.use(express.bodyParser())

    // Set up routes used by count-von-count
    var cvcRoutes = {
        '/config'           : configHandler,
        '/:teamid/position' : positionHandler,
        '/:teamid/laps'     : lapsHandler
    }
    for(route in cvcRoutes) {
        server.put(route, auth.cvcAuth, cvcRoutes[route])
    }

    // Run the server
    server.listen(port)
}
exports.run = run

// Temporary, better state management is next
var config = {}

var configHandler = function(req, res) {
    config = req.body
    res.send(200);
}

var positionHandler = function(req, res) {
    server.getClient().publish('/position',{
        team: {
            id: req.body.team.id,
            name: req.body.team.name,
            laps: req.body.team.laps
        },
        speed: req.body.speed,
        station: {
            position: req.body.station.position,
            name: req.body.station.name
        }
    });
    res.send(200)
}

var lapsHandler = function(req, res) {
    server.getClient().publish('/laps', req.body);
    res.send(200);
}
