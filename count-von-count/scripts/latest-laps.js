function pad(number, length) {
  var str = '' + number;
  while (str.length < length) {
    str = '0' + str;
  }
  
  return str;
}

function formatTime(time) {
  return pad(time.getHours(), 2) + ':' +
    pad(time.getMinutes(), 2) + ':' +
    pad(time.getSeconds(), 2);
}

db.teams.find({}, {'laps_': {'$slice': -5}}).forEach(function(team) {
  print(team.name + ':');
  for(var i in team.laps_) {
    var lap = team.laps_[i];
    print('  ' + lap.count + ', ' + lap.reason + ', ' +
      formatTime(lap.timestamp));
  }
});
