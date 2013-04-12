var stupidInterpolator = function(boxxy) {
    var lastPosition = {};

    this.init = function(teamId, timestamp, distance) {
        lastPosition[teamId] = {
            timestamp: timestamp,
            distance: distance,
            speed: 4.5
        }
    };

    this.update = function(teamId, timestamp, distance) {
        var previousPosition = lastPosition[teamId],
            deltaTime = (timestamp - previousPosition.timestamp) / 1000,
            distanceTravelled = distance - previousPosition.distance;
        if(distanceTravelled < 0) distanceTravelled += boxxy.circuitLength;

        lastPosition[teamId] = {
            timestamp: timestamp,
            distance: distance,
            speed: distanceTravelled / deltaTime
        }
    };

    this.getSpeed = function(teamId, timestamp) {
        return lastPosition[teamId].speed;
    };

    this.getPosition = function(teamId, timestamp) {
        var position = lastPosition[teamId],
            deltaTime = (timestamp - position.timestamp) / 1000,
            estimate = position.distance + position.speed * deltaTime;
        return estimate % boxxy.circuitLength;
    }
};
