/**
 * Een kleine proof of concept voor Boxxy met Faye
 */

var http = require('http'),
    faye = require('faye'),
    express = require('express');

var port = 8080
var server = new faye.NodeAdapter({mount: '/boxxy', timeout: 45})

server.addExtension({
    incoming: function(message, callback) {
        // Hier kunnen inkomende messages van de views tegengehouden worden,
        // zodat ze geen foute informatie kunnen publishen
        callback(message)
    }
})

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

app.put('/:teamid/position',function(req,res){
    console.log("teamid logged");
    res.send(200)

    if(req.params.key == "tetten"){
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
    server.getClient().publish('/' + rec.params.teamid + '/position');
    }else{
	res.send(404)
    }
    

});

app.put('/:teamid/laps', function(req, res) {
    console.log('lap logged')
    server.getClient().publish('/:' + rec.params.teamid + '/laps');
    res.send(200);
})

app.put('/config', function(req, res) {
    console.log('config!')
    res.send(200);
})

server.attach(app)
app.listen(port)
console.log("Boxxy running on port " + port)
