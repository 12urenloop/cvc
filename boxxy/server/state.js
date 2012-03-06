// TODO: change to a realistic running speed
// var STARTING_SPEED = 1.0 // Dikke van Turing
// var STARTING_SPEED = 2.0 // Michiel Van den Berghe
var STARTING_SPEED = 4.0 // USAIN BOLT!!!


var state = {
    teams: {}
}

exports.initialize = function(config) {
    state.stations = config.stations
    for(var teamIdx in config.teams) {
        var team = config.teams[teamIdx]
        team.speed = STARTING_SPEED
        team.station = state.stations[0]
        state.teams[team.id] = team
    }
}

exports.updatePosition = function(message) {
    state.teams[message.team.id].station = message.station
    state.teams[message.team.id].speed = message.speed
}

exports.updateLaps = function(message) {
    state.teams[message.team.id].laps = message.team.laps
}

exports.get = function() {
    return state
}
