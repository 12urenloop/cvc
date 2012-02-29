/**
 * Een kleine proof of concept voor Boxxy met Faye
 */

var http = require('http'),
    faye = require('faye'),
    express = require('express')
    
var auth = require('./auth')
console.log(auth)
console.log(auth.ServerAuth)
var port = 8080
var server = new faye.NodeAdapter({mount: '/boxxy', timeout: 45})

server.addExtension(auth.ServerAuth)
server.getClient().addExtension(auth.ClientAuth)

var app = express.createServer()
app.configure(function(){
    app.use(express.bodyParser())
})

app.put('/cvc/position', function(req, res){
    console.log('position!')
    server.getClient().publish('/checkpoint', {
        type: 'checkpoint',
        checkpoint: req.body.position,
        name: req.body.team,
        speed: req.body.speed
    })
    res.send({status: 'ok'})
})

app.all('/', function(req, res) {
    console.log("test/");
    res.send(200);
})

app.all('/:teamid/position',function(req,res){
    console.log("teamid logged");
    res.send(200)
    if(req.query.key == "tetten"){
    server.getClient().publish('/position/',{
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
    server.getClient().publish('/' + req.params.teamid + '/position');
    }else{
	res.send(404)
    }
    

});

app.all('/:teamid/laps', function(req, res) {
    console.log('lap logged')
    server.getClient().publish('/:' + req.params.teamid + '/laps');
    res.send(200);
})

app.all('/config', function(req, res) {
    console.log('config!')
    res.send(200);
})

server.attach(app)
app.listen(port)
console.log("Boxxy running on port " + port)
