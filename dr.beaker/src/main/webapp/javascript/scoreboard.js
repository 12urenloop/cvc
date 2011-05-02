
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
            console.log("removing team " + data.team[i].id);
            $("#team-" + data.team[i].id).remove();
            rows.insertBefore("<tr id=\"team-"+data.team[i].id+"\"><td>"+data.team[i].name+'</td><td class="score">'+data.team[i].score+"</td></tr>")
          }
          else {
            j++;
          }

        }
          	
        /*// remove the old data
        scoreboard.find('tbody tr').remove();

        body = $('#scoreboard tbody')

        // add the new data
        for (i = 0; i < data.team.length; i++) {
          body.append("<tr><td>"+data.team[i].name+'</td><td class="score">'+data.team[i].score+"</td></tr>");
        }

        // set the scoreboard visible again
        scoreboard.show();*/
      }
    });
  }

  this.toggleAutoRefresh = function () {
    if (!autoRefresh) {
      // set the scoreboard to refresh every REFRESH_TIMEOUT milliseconds
      interval = setInterval(this.update, REFRESH_TIMEOUT);
    } else {
      clearInterval(interval);
    }
    autoRefresh = !autoRefresh;
  }
}

$(function() {
  scoreboard = new ScoreBoard().init();
})
