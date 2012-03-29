function Interpolation() {
    var self = this;

    this.init = function(config) {
        self.stations = {};

        for(var stationIdx in config.stations) {
            var station = config.stations[stationIdx];
            self.stations[station.name] = station;
        }
        self.teams = {}
        for(var teamIdx in config.teams) {
            var team = config.teams[teamIdx];
            self.teams[team.id] = {
                id: team.id,
                name: team.name,
                speed: team.speed,
                station: team.station,
                time: new Date(team.time),
                lastPosition: team.station.position
            }
        }

        self.circuitLength = config.circuitLength;
        self.timeOffset = 0;
    }

    this.update = function(message) {
        var team = self.teams[message.team.id],
            predictedPosition = self.getPosition(team.id, new Date(message.time)),
            actualPosition = message.station.position,
            targetPosition = self.stations[message.station.name].next.position,
            targetTime = self.mod(targetPosition - actualPosition + self.circuitLength) / message.speed,
            targetSpeed = self.mod(targetPosition - predictedPosition + self.circuitLength) / targetTime;
        team.lastPosition = predictedPosition;
        team.time = new Date(message.time);
        team.speed = targetSpeed;
        team.station = message.station;
        // Only correct time once. Can cause jumps otherwise.
        // TODO: use timestamp from config message
        if(self.timeOffset == 0) {
            self.timeOffset = (new Date()) - team.time;
        }
    }

    this.getPosition = function(teamid, time) {
        if(time == undefined) {
            time = (new Date() - self.timeOffset);
        }
        var team = self.teams[teamid],
            distanceTravelled = (time - team.time) / 1000 * team.speed;
        return self.mod(team.lastPosition + distanceTravelled);
    }

    this.getCoords = function(teamid, shape, time) {
        if(shape == undefined) {
            shape = shapes.line;
        }
        var pos = self.getPosition(teamid, time);
        return shape(pos / self.circuitLength);
    }
    
    this.getSpeed = function(teamid) {
        return self.teams[teamid].speed;
    }

    // Floating point modulo
    this.mod = function(position) {
        while(position > self.circuitLength) {
            position -= self.circuitLength;
        }
        return position;
    }
}

var shapes = {
    // returns a simple line normalized to [0, 1]
    line: function(pos) {
        return {
            x: pos,
            y: 0
        };
    },
    // Example of a slightly more complex circuit: a rectangle with width 1
    rect: function(pos) {
        var ratio = 1.61803, // It's art!
            width = 1.0,
            height = 1.0 / ratio,
            total = 2 * (width + height),
            progress = pos * total;
        // Bottom
        if(progress < width) {
            return {
                x: progress,
                y: 0
            };
        // Right
        } else if(progress < width + height) {
            return {
                x: width,
                y: progress - width
            };
        // Top
        } else if(progress < 2 * width + height) {
            return {
                x: width - (progress - width - height),
                y: height
            }
        // Left
        } else {
            return {
                x: 0,
                y: total - progress
            }
        }
    } 
}