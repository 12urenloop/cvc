
// refresh timeout is 5 minutes
REFRESH_TIMEOUT = 5 * 60 * 1000

function ScoreBoard () {

  var that = this;
  var scoreboard = $('#scoreboard');

  var autoRefresh = false;
  var interval;

  this.init = function () {
    $('#autorefresh-toggle').click(this.toggleAutoRefresh);
    this.toggleAutoRefresh();
  }

  this.toggleAutoRefresh = function () {
    if (!autoRefresh) {
      // set the scoreboard to refresh every REFRESH_TIMEOUT milliseconds
      interval = setInterval(function() {that.refresh()}, REFRESH_TIMEOUT / 60);
    } else {
      clearInterval(interval);
    }
    autoRefresh = !autoRefresh;
  }

  this.update = function () {
    // fetch the new data
    $.ajax({
      type:'GET',
      url:'/dr.beaker/api/teams/',
      data:'order=score',
      success:function (data) {
        // remove the old data
        scoreboard.find('tbody tr').remove();

        body = $('#scoreboard tbody')

        // add the new data
        for (i = 0; i < data.team.length; i++) {
          body.append("<tr><td>"+data.team[i].name+'</td><td class="score">'+data.team[i].score+"</td></tr>");
        }

        // set the scoreboard visible again
        scoreboard.show();
      }
    });
  }

  this.refresh = function () {
    // hide the scoreboard, update when hide animation is done
    scoreboard.hide(this.update);
  }
}

$(function() {
  scoreboard = new ScoreBoard().init();
})
