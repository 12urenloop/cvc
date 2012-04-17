var express = require('express'),
    faye = require('faye'),

    auth = require('./auth'),
    state = require('./state')

var server, bayeux
var run = function(port) {
    server = express.createServer(),
    bayeux = new faye.NodeAdapter({ mount: '/boxxy', timeout: 45 })
    bayeux.attach(server)

    // Configure the faye server to use the correct keys
    bayeux.addExtension(auth.serverAuth)
    bayeux.getClient().addExtension(auth.clientAuth)

    // Parse messages as JSON
    server.use(express.bodyParser())
    server.use(express.static(__dirname + '/../public'))

    // Set up routes used by count-von-count
    var cvcRoutes = {
        '/config'           : configHandler,
        '/:teamid/position' : positionHandler,
        '/:teamid/laps'     : lapsHandler
    }
    for(route in cvcRoutes) {
        server.put(route, auth.cvcAuth, cvcRoutes[route])
    }

    // Information to initialize clients
    server.get('/init', initHandler)

    // Run the server
    server.listen(port)
}
exports.run = run

var configHandler = function(req, res) {
    console.log('[CONFIG]', req.body)
    state.initialize(req.body)
    res.send(200)
}

var positionHandler = function(req, res) {
    console.log('[POSITION]', req.body)
    state.updatePosition(req.body)
    bayeux.getClient().publish('/position',{
        time: req.body.time,
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
    })
    res.send(200)
}

var lapsHandler = function(req, res) {
    console.log('[LAP]', req.body)
    state.updateLaps(req.body)
    bayeux.getClient().publish('/laps', req.body)
    res.send(200)
}

var initHandler = function(req, res) {
    // required for cross-domain requests
    res.header('Access-Control-Allow-Origin', '*')
    res.send(JSON.stringify(state.get()))
}
