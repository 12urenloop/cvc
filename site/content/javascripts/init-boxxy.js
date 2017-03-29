function scrollTo(target){
  var targetPosition = $(target).offset().top;
  $('html,body').animate({ scrollTop: targetPosition}, 'slow');
}

$(document).ready(function(){
  $('nav').sticky({topSpacing:0});

  $('.flexslider').flexslider({
    animation: "slide",
    slideshow: true,
    slideshowSpeed: 5000,
    animationSpeed: 1000
  });

  $("a[rel^='prettyPhoto']").prettyPhoto({
    social_tools: false,
    theme: 'light_square'
  });
});

var boxxy = boxxy.initialize(),
    container = $('#live-results table');
boxxy.onUpdate = function() {
  container.empty();
  var teams = boxxy.teamsByScore();
  if(teams.length >= 6) {
    var row = $('<tr id="teams3">');
    row.append('<td>' + (teams[0].rankingPosition+1) + '. ' + teams[0].name);
    row.append('<td>' + (teams[1].rankingPosition+1) + '. ' + teams[1].name);
    row.append('<td>' + (teams[2].rankingPosition+1) + '. ' + teams[2].name);
    row.append('<td rowspan="4" class="follow-link">Volg alles live op <a href="https://live.12urenloop.be">live.12urenloop.be');
    container.append(row);

    var row = $('<tr id="laps3">');
    row.append('<td>' + teams[0].laps + ' rondjes');
    row.append('<td>' + teams[1].laps + ' rondjes');
    row.append('<td>' + teams[2].laps + ' rondjes');
    container.append(row);

    var row = $('<tr id="teams6">');
    row.append('<td>' + (teams[3].rankingPosition+1) + '. ' + teams[3].name);
    row.append('<td>' + (teams[4].rankingPosition+1) + '. ' + teams[4].name);
    row.append('<td>' + (teams[5].rankingPosition+1) + '. ' + teams[5].name);
    container.append(row);

    var row = $('<tr id="laps6">');
    row.append('<td>' + teams[3].laps + ' rondjes');
    row.append('<td>' + teams[4].laps + ' rondjes');
    row.append('<td>' + teams[5].laps + ' rondjes');
    container.append(row);

    var row = $('<tr class="follow-link-bottom">');
    row.append('<td colspan="3">Volg alles live op <a href="https://live.12urenloop.be">live.12urenloop.be');
    container.append(row);
  }
}
boxxy.listen('https://live.12urenloop.be');

/* Livestream player */
function initPlayer() {
  jwplayer('mediaspace').setup({
    'id' : 'playerID',
    'width': '620',
    'height':'350',
    'file' : 'rtmp://wowza1.ugent.be/live/12urenloop'
  });
}
$(".play-button a").prettyPhoto({
  custom_markup: '<div id="mediaspace" style="height:350px;width:620px;"></div>',
  changepicturecallback: initPlayer
});

/* Google Analytics */
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-25444917-4', 'auto');
ga('send', 'pageview');
