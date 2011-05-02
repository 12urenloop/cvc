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
var refreshtime = 30;

// on DOM ready	
$(document).ready(function (){		
  $("#wrapper").css('font-size', ($(window).width() * 0.018));
  $("#imglogo").css('width', ($(window).width() * 0.14));

  $(window).resize(function() {
    $("#wrapper").css('font-size', ($(window).width() * 0.018));
    $("#imglogo").css('width', ($(window).width() * 0.14));
  });

  var minwidth = 50;
  var maxwidth = 90;
  var maximum = $('li').attr("rel")*1;

  //$('li').each(function(){
    //totaal = (totaal*1 + $(this).attr("rel")*1);
  //});

  $('li').each(function(){
    var breedte = (minwidth * $(this).attr('rel') / maximum) + (maxwidth - minwidth)*1 + '%'; 
    $(this).animate({
      width: breedte,
    }, 3000); 

  });

  //Scroller
  // Set all items hidden
  // Set first item visible
  var tickerTime = 5; //tijd in seconden.

  function init(selector){
    //make first item visible
    $(selector +':first').addClass('visible').fadeIn('slow');
    //start ticker
    setTimeout(function(){nextItem(selector)}, tickerTime * 1000);


  }
  //Set second item visible under first
  //hide n-th and unhide (n+1)th.
  function nextItem(selector){
    //check if this item is last item()
    if($(selector + ':last').hasClass('visible')){
      //put first item visible
      $(selector).addClass('visible').fadeIn('slow');
      $(selector + ':last').removeClass('visible').fadeOut('slow');;
    }else{
      $(selector).nextAll(selector).addClass('visible').fadeIn('slow');
      $(selector+':first').removeClass('visible').fadeOut('slow');
    }	
    setTimeout(function(){nextItem(selector)}, tickerTime * 1000);
  }
  //start de cascade
  init('div.hoofdsponsor');
  setTimeout(function(){init('div.cafe');}, tickerTime / 2* 1000);


  //progressbar for refresh
  var refreshTime = 30; // in seconden, ahja.
  var currentTime = 0*1;
  $("#progress").animate({width: "100%"}, refreshTime * 1000, 'linear');
  setTimeout(function(){location.reload();}, refreshTime * 1000);

  function refresh(){
    location.reload();
  }
});

$(function() {
  scoreboard = new ScoreBoard().init();
})