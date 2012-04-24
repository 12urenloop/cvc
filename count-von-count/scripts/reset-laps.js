db.teams.find().forEach(function(team) {
  print("Resetting " + team.name + " (" + team.laps + " laps)");
  db.teams.update({"_id": team._id}, {"$set": {"laps": 0, "laps_": []}});
});
