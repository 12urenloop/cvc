// TODO: reevaluate this code

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
