db.teams.find().forEach(function(team) {
  print(team.name + ": " + team.laps);
});
