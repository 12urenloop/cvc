function Scoreboard() {
  var self = this;
  var scoreboard = $('#scoreboard');

  var createBar = function(team) {
    var o = $('<div>').attr('id', 'team-' + team.id);

    return o;
  }

  var updateBar = function(bar, team) {

  }

  this.init = function(config) {
    for(team in config.teams) {
      console.log(config.teams[team])
      console.log(createBar(config.teams[team]));
    }
  }

  this.update = function(data) {
    console.log(data);
  }
}

// Don't use $(), Boxxy is not yet loaded then
window.onload = function() {
  var scoreboard = new Scoreboard();
  var client = new Boxxy();
  client.receivedConfig = scoreboard.init;
  client.addedLap = scoreboard.update;
  client.connect();
}
