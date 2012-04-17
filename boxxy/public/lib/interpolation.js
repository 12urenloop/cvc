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
                lastPosition: team.station.position,
                correction: 0
            }
        }

        self.circuitLength = config.circuitLength;
        self.timeOffset = 0;
    }

    this.update = function(message) {
        // Only correct time once. Can cause jumps otherwise.
        // TODO: use timestamp from config message
        if(self.timeOffset == 0) {
            console.log(message.time);
            self.timeOffset = new Date() - new Date(message.time);
            console.log(self.timeOffset);
        }
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
    }

    this.getPosition = function(teamid, time) {
        if(time == undefined) {
            time = new Date();
        }
        time -= self.timeOffset;
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
            height = 1.0 / ratio;
            
        return shapes.segmented(pos, [
            {x: 0, y: 0},
            {x: width, y: 0},
            {x: width, y: height},
            {x: 0, y: height}
        ]);
    },
    
    // Approximation of the real octagonal circuit
    real: function(pos) {
        return shapes.segmented(pos, [
            {x: 0, y: 0.07},
            {x: 0.1, y: 0},
            {x: 0.9, y: 0},
            {x: 1, y: 0.07},
            {x: 1, y: 0.28},
            {x: 0.9, y: 0.35},
            {x: 0.1, y: 0.35},
            {x: 0, y: 0.28}
        ]);
    },
    
    segmented: function(pos, segments) {
        var totalLength = 0;
        // make it a linked list, store length of each segment and
        // calculate total circuit length.
        for(var idx = 0; idx < segments.length; idx++) {
            var last = segments[idx],
                next = segments[(idx + 1) % segments.length];
            last.length = Math.sqrt((next.x - last.x) * (next.x - last.x) + (next.y - last.y) * (next.y - last.y));
            last.next = next;
            totalLength += last.length;
        }
        
        var progressLeft = totalLength * pos,
            segment = segments[0];

        while(progressLeft > segment.length) {
            progressLeft -= segment.length;
            segment = segment.next;
        }
        
        var diffY = (segment.next.y - segment.y),
            diffX = (segment.next.x - segment.x),
            segmentPercentage = progressLeft / segment.length;
        return {
            x: segment.x + segmentPercentage * diffX,
            y: segment.y + segmentPercentage * diffY
        }
    }
}
