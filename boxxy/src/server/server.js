/**
 * Een kleine proof of concept voor Boxxy met Faye
 */

var http = require('http'),
    faye = require('faye'),
    express = require('express');

var port = 9000
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
    
})
server.attach(app)
app.listen(port)
console.log("Boxxy running on port " + port)