exports.initialize = function() {
    state = {};
    state.teams = {};
    state.laps = [];
    state.maxLaps = 10;

    state.onUpdate = function(state) {};
    state.onPutState = function(state) {};
    state.onAddLap = function(state, lap) {};

    return state;
}

exports.putState = function(state, newState) {
    state.teams = newState.teams;
    state.laps = newState.laps;

    state.onUpdate(state);
    state.onPutState(state);
}

exports.addLap = function(state, lap) {
    state.laps = [lap].concat(state.laps);
    if(state.laps.length > state.maxlaps) state.laps.shift();

    state.teams[lap.team.id] = lap.team;

    state.onUpdate(state);
    state.onAddLap(state, lap);
}
