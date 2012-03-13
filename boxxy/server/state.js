// TODO: change to a realistic running speed
// var STARTING_SPEED = 1.0 // Dikke van Turing
// var STARTING_SPEED = 2.0 // Michiel Van den Berghe
var STARTING_SPEED = 4.0 // USAIN BOLT!!!

var state = {
    stations: [],
    teams:    {}
}

exports.initialize = function(config) {
    state.stations = config.stations

    for(var teamIdx in config.teams) {
        var team = config.teams[teamIdx]
        team.lastLapCompleted = new Date()
        team.speed = STARTING_SPEED
        team.station = state.stations[0]
        state.teams[team.id] = team
    }
    
    state.circuitLength = config.circuitLength
}

exports.updatePosition = function(message) {
    var team = state.teams[message.team.id]
    team.station = message.station
    team.speed = message.speed
    team.time = message.time
}

exports.updateLaps = function(message) {
    var team = state.teams[message.team.id]
    var previousLap = team.lastLapCompleted
    team.lastLapCompleted = new Date(message.time)
    team.laps = message.team.laps
    message.lapTime = Math.round((team.lastLapCompleted - previousLap) / 1000)
}

exports.get = function() {
    return state
}
