exports.Runner = Runner

var events = require('events'),
    util = require('util')

function Runner(id, name) {
    events.EventEmitter.call(this)
    this.id = id
    this.name = name
    this.station = 0
    this.position = 0
    this.laps = 0
    this.segmentSize = 100
    this.stations = 4

    this.start = function() {
        var self = this
        self.updateSpeed()
        setInterval(function() {
            self.position += self.speed / 10
            if(self.position >= self.segmentSize) {
                self.position = 0
                self.station = (self.station + 1) % self.stations
                self.emit('position', {
                    team: {
                        id: self.id,
                        name: self.name,
                        laps: self.laps
                    },
                    speed: self.speed,
                    station: {
                        position: self.station,
                        name: 'gyrid-' + (self.id + 1)
                    }
                })
                // New speed per segment
                self.updateSpeed()
                if(self.station == 0) {
                    self.laps++
                    self.emit('laps', {
                        count: 1,
                        team: {
                            id: self.id,
                            name: self.name,
                            laps: self.laps
                        }
                    })
                }
            }
        }, 100)
    }
    
    this.updateSpeed = function() {
        var speed = 0
        for(var it = 0; it < 10; it++) {
            speed += Math.random()
        }
        this.speed = speed * 5
    }
}
util.inherits(Runner, events.EventEmitter)
