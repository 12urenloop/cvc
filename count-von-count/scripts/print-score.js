var i = 1;
db.teams.find().sort({'laps': -1}).forEach(function(team) {
  print(i + '. ' + team.name + ' (' + team.laps + ')');
  i++;
});
