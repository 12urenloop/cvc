function Boxxy() {
    /* Connection */
    this.socket = null;

    /* State */
    this.frozen        = false;
    this.frozenScore   = null;
    this.notification  = null;
    this.circuitLength = 0;
    this.startTime     = null;
    this.stations      = {};
    this.teams         = {};
    this.laps          = [];
    this.maxLaps       = 10;

    /* To be user will override this */
    this.onPutState       = function(stateDelta) {};
    this.onAddLap         = function(lap) {};
    this.onUpdatePosition = function(position) {};
    this.onUpdate         = function() {};
}

Boxxy.prototype.putState = function(stateDelta) {
    if(stateDelta.notification != null) this.notification = stateDelta.notification;
    if(stateDelta.circuitLength != null) this.circuitLength = stateDelta.circuitLength;
    if(stateDelta.startTime != null) this.startTime = stateDelta.startTime;
    if(stateDelta.stations != null) this.stations = stateDelta.stations;

    // This signifies the change melted -> frozen
    if(stateDelta.frozen != null && stateDelta.frozen && !this.frozen) {
        this.frozenScore = this.teamsByScore();
        this.frozen = true;
    }

    // This signifies the change frozen -> melted
    if(stateDelta.frozen != null && !stateDelta.frozen && this.frozen) {
        this.frozen = false;
    }

    if(!this.frozen && stateDelta.teams) this.teams = stateDelta.teams;
    if(!this.frozen && stateDelta.laps) this.laps = stateDelta.laps;

    this.onPutState(stateDelta);
    this.onUpdate();
}

Boxxy.prototype.addLap = function(lap) {
    if(this.laps.length >= this.maxLaps) this.laps.pop();
    this.laps = [lap].concat(this.laps);

    /* Just copy the total laps instead of calculating it, more robust. */
    this.teams[lap.team].laps = lap.total;
    this.teams[lap.team].updated = lap.timestamp;

    this.onAddLap(lap);
    this.onUpdate();
}

Boxxy.prototype.updatePosition = function(position) {
    this.teams[position.team].station = position.station;
    this.teams[position.team].updated = position.timestamp;
    this.onUpdatePosition(position);
    this.onUpdate();
}

Boxxy.prototype.teamsByScore = function() {
    var result = [];
    for(var i in this.teams) result.push(this.teams[i]);

    // Sort, first by #laps, then by position
    var stations = this.stations;
    result.sort(function(a, b) {
        var order = b.laps - a.laps;
        if(order == 0) {
            var stationA = stations[a.station],
                stationB = stations[b.station];
            if(stationA && stationB) {
                order = stationB.position - stationA.position;
            }
        }
        return order;
    });

    for (var i = 0; i < result.length; i++) {
        result[i].rankingPosition = i;
    }
    return result;
}

/* Only used on the client side: this requires sockets.io to be in scope. */
Boxxy.prototype.listen = function(uri) {
    var boxxy = this;
    boxxy.socket = io.connect(uri);

    boxxy.socket.on('/state', function(state) {
        boxxy.putState(state);
    });

    boxxy.socket.on('/lap', function(lap) {
        boxxy.addLap(lap);
    });

    boxxy.socket.on('/position', function(position) {
        boxxy.updatePosition(position);
    });
}

exports.initialize = function() {
    return new Boxxy();
}
