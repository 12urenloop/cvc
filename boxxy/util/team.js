exports.Team = Team
exports.ISODateString = ISODateString
var events = require('events'),
    util = require('util')

// From Mozilla Developer Network
function ISODateString(d){
  function pad(n){return n<10 ? '0'+n : n}
  function padm(n){return (n < 100 ? '0' : '') + pad(n)}
  return d.getUTCFullYear()+'-'
      + pad(d.getUTCMonth()+1)+'-'
      + pad(d.getUTCDate())+'T'
      + pad(d.getUTCHours())+':'
      + pad(d.getUTCMinutes())+':'
      + pad(d.getUTCSeconds())+'.'
      + padm(d.getUTCMilliseconds()) + 'Z'
}

function Team(id, name, config) {
    events.EventEmitter.call(this)
    this.id = id
    this.name = name
    this.config = config
    this.stationIdx = 0
    this.laps = 0
    var self = this

    this.run = function() {
        var speed = self.nextSpeed(),
            lastSegment = (self.stationIdx == self.config.stations.length - 1),
            nextPos = lastSegment ? self.config.circuitLength : self.nextStation().position,
            length = nextPos - self.station().position
        setTimeout(function() {
            self.proceed()
            self.emit('position', {
                time: ISODateString(new Date()),
                team: {
                    id: self.id,
                    name: self.name,
                    laps: self.laps
                },
                speed: speed,
                station: self.station()
            })
            if(lastSegment) {
                self.laps++
                self.emit('laps', {
                    time: ISODateString(new Date()),
                    count: 1,
                    team: {
                        id: self.id,
                        name: self.name,
                        laps: self.laps
                    }
                })
            }
            self.run()
        }, length / speed * 1000)

    }

    this.station = function() {
        return this.config.stations[this.stationIdx]
    }

    this.nextStation = function() {
        return this.config.stations[(this.stationIdx + 1) % this.config.stations.length]
    }

    this.nextSpeed = function() {
        var speed = 0
        for(var it = 0; it < 10; it++) {
            speed += Math.random()
        }
        return speed // * 5 // if you want to speed up debugging
    }

    this.proceed = function() {
        self.stationIdx = (self.stationIdx + 1) % self.config.stations.length
    }

}
util.inherits(Team, events.EventEmitter)
