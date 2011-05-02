// refresh timeout is 5 seconds
REFRESH_TIMEOUT = 5 * 1000

function ScoreBoard () {

  var that = this;
  var scoreboard = $('#scoreboard');

  var autoRefresh = false;
  var interval;

  this.init = function () {
    $('#autorefresh-toggle').click(this.toggleAutoRefresh);
    this.toggleAutoRefresh();
  }

  this.update = function () {
    // fetch the new data
    $.ajax({
      type:'GET',
      url:'/dr.beaker/api/teams/',
      data:'order=score',
      success:function (data) {
        var rows = $('tr', scoreboard);
        for (i = 0, j = 0; i < data.team.length; i++) {
          var row = $(rows[j]);
          if ("team-" + data.team[i].id != row.attr('id')) {
            var removed = $("#team-" + data.team[i].id).remove()[0];
            rows = $.grep(rows, function(el) { return el != removed });
            row.before("<tr id=\"team-"+data.team[i].id+"\"><td>"+data.team[i].name+'</td><td class="score">'+data.team[i].score+"</td></tr>")
          }
          else {
            $('.score', row).text(data.team[i].score);
            j++;
          }
        }
      }
    });
  }

  this.toggleAutoRefresh = function () {
    if (!autoRefresh) {
      $('#autorefresh-toggle').text('Autorefresh is ON');
      // set the scoreboard to refresh every REFRESH_TIMEOUT milliseconds
      interval = setInterval(that.update, REFRESH_TIMEOUT);
    } else {
      $('#autorefresh-toggle').text('Autorefresh is OFF');
      clearInterval(interval);
    }
    autoRefresh = !autoRefresh;
    return false;
  }
}

$(function() {
  scoreboard = new ScoreBoard().init();
})