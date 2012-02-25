exports.Runner = Runner

events = require('events')
util = require('util')

function Runner(name) {
    events.EventEmitter.call(this)
    this.checkpoint = 0
    this.name = name
    this.position = 0
    // snelheid in radialen. Drie keer random zodat het een normaalverdeling
    // benadert :)
    this.speed = (Math.random() + Math.random() + Math.random()) / 3
    
    this.start = function() {
        var self = this
        setInterval(function() {
            self.position += self.speed / 10
            if(self.position >= Math.PI / 2) {
                self.position = 0
                self.checkpoint = (self.checkpoint + 1) % 4
                self.emit('checkpoint', self, self.checkpoint, self.speed)
                // Nieuwe snelheid per ronde
                self.speed = (Math.random() + Math.random() + Math.random()) / 3
            }
        }, 100)
    }
}
util.inherits(Runner, events.EventEmitter)