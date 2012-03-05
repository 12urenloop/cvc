/**
 * Een kleine proof of concept voor Boxxy met Faye
 */

var express = require('express'),
    faye = require('faye'),
    http = require('http'),

    auth = require('./auth')

// Temporary, better state management is next
var config = {}

var port = 8080
var server = new faye.NodeAdapter({mount: '/boxxy', timeout: 45})

// Makes the server check for the publishing key
server.addExtension(auth.serverAuth)
// Makes the server's client add the publishing key
server.getClient().addExtension(auth.clientAuth)

var app = express.createServer()
app.configure(function() {
    // Parses JSON body
    app.use(express.bodyParser())
});

//                            /- Express middleware to authenticate cvc
app.put('/:teamid/position', auth.cvcAuth, function(req,res){
    console.log("position")
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
});

app.put('/:teamid/laps', auth.cvcAuth, function(req, res) {
    console.log('lap')
    server.getClient().publish('/laps', req.body);
    res.send(200);
})

app.put('/config', auth.cvcAuth, function(req, res) {
    console.log('config')
    config = req.body
    res.send(200);
})

server.attach(app)
app.listen(port)
console.log("Boxxy running on port " + port)
