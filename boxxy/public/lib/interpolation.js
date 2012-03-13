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
        self.timeOffset = (new Date()) - team.time;
    }

    this.getPosition = function(teamid, time) {
        if(time == undefined) {
            time = (new Date() - self.timeOffset);
        }
        var team = self.teams[teamid],
            distanceTravelled = (time - team.time) / 1000 * team.speed;
        return self.mod(team.lastPosition + distanceTravelled);
    }
    
    this.getSpeed = function(teamid) {
        return self.teams[teamid].speed;
    }
    
    this.mod = function(position) {
        while(position > self.circuitLength) {
            position -= self.circuitLength;
        }
        return position;
    }
}