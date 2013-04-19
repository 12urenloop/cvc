var averageInterpolator = function(boxxy) {
    var interstationSpeeds = {},
        speedCompensations = {},
        previousPositions = {},
        runnerFactor = 0.5;

    this.init = function(teamId, timestamp, position) {
        interstationSpeeds[teamId] = {};
        speedCompensations[teamId] = 1;
        previousPositions[teamId] = { 'position': position, 'timestamp': timestamp };

        for (var stationId in boxxy.stations) {
            var station = boxxy.stations[stationId]
            interstationSpeeds[teamId][station.position] = new CBuffer(5);
            interstationSpeeds[teamId][station.position].push(5);
        }
    };

    this.getDistance = function(positionA, positionB) {
        var distance = positionB - positionA;
        if(distance <= 0) distance += boxxy.circuitLength;
        return distance;
    }

    this.getNextStation = function(lastPosition) {
        // Find first station position higher
        var nextStation, minDistance;

        for (var stationId in boxxy.stations) {
            var station = boxxy.stations[stationId],
                distance = this.getDistance(lastPosition, station.position);
            if(!nextStation || distance < minDistance) {
                nextStation = station;
                minDistance = distance;
            }
        }

        return nextStation;
    }

    this.update = function(teamId, timestamp, position) {
        var previousPosition = previousPositions[teamId],
            deltaTime = (timestamp - previousPosition.timestamp) / 1000,
            distance = this.getDistance(previousPosition.position, position),
            speed = distance / deltaTime;

        var averageStationSpeed = interstationSpeeds[teamId][position].avg(),
            speedCompensation = speed / averageStationSpeed;
        speedCompensations[teamId] = 1 - runnerFactor * (1 + speedCompensation);

        interstationSpeeds[teamId][position].push(speed);
        previousPositions[teamId] = { 'position': position, 'timestamp': timestamp };
    };

    this.getSpeed = function(teamId, timestamp) {
        var nextStation = this.getNextStation(previousPositions[teamId].position),
            nextSpeed = interstationSpeeds[teamId][nextStation.position].avg();
        return nextSpeed;
    };

    this.getPosition = function(teamId, timestamp) {
        var previousPosition = previousPositions[teamId],
            deltaTime = (timestamp - previousPosition.timestamp) / 1000,
            currentSpeed = this.getSpeed(teamId, timestamp),
            estimate = previousPosition.position + currentSpeed * deltaTime;
        return estimate % boxxy.circuitLength;
    }
};
