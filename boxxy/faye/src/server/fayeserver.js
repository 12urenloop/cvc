/**
 * Een kleine proof of concept voor Boxxy met Faye
 */

var http = require('http'),
    faye = require('faye');

var runner = require('./runner')
var port = 9000
var server = new faye.NodeAdapter({mount: '/boxxy', timeout: 45})

server.addExtension({
    incoming: function(message, callback) {
        // Hier kunnen inkomende messages van de views tegengehouden worden,
        // zodat ze geen foute informatie kunnen publishen
        callback(message)
    }
})

var names = ["Michiel", "Sander", "Pieter"]
var runners = []
// We maken een paar dummy lopers aan, en publishen wanneer ze een checkpoint passeren.
for(var idx in names) {
    r = new runner.Runner(names[idx])
    r.on('checkpoint', function(runner, checkpoint, speed) {
        console.log('Checkpoint ' + checkpoint + ' : ' + runner.name + ' @ ' + speed + ' rad/s')
        server.getClient().publish('/checkpoint', {
            type: 'checkpoint',
            checkpoint: checkpoint,
            name: runner.name,
            speed: speed
        })
    })
    r.start()
}

server.listen(port);
console.log("Boxxy running on port " + port)