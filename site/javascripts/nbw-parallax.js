/*
Demo: Despiration Tutorial Parallax Demo
Author: Elias Ghosn - Despiration.com
Author URL: http://www.despiration.com/
Tutorial URL: http://www.ianlunn.co.uk/blog/code-tutorials/recreate-nikebetterworld-parallax/

License: http://creativecommons.org/licenses/by-sa/3.0/ (Attribution Share Alike). Please attribute work to Despiration.com simply by leaving these comments in the source code or if you'd prefer, place a link on your website to http://www.despiration.com/.

Dual licensed under the MIT and GPL licenses:
http://www.opensource.org/licenses/mit-license.php
http://www.gnu.org/licenses/gpl.html
*/

$(document).ready(function() { //when the document is ready...


	//save selectors as variables to increase performance
	var $window = $(window);
	var $firstBG = $('#intro');
	var bg1 = $("#intro .bg1");
	var $secondBG = $('#separator1');
	var bg2 = $("#separator1 .bg2");
	var $thirdBG = $('#separator2');
	var bg3 = $("#separator2 .bg3");
	var $fourthBG = $('#separator3');
	var bg4 = $("#separator3 .bg4");

	var windowHeight = $window.height(); //get the height of the window


	//apply the class "inview" to a section that is in the viewport
	$('#intro, #separator1, #separator2, #separator3').bind('inview', function (event, visible) {
			if (visible == true) {
			$(this).addClass("inview");
			} else {
			$(this).removeClass("inview");
			}
		});


	//function that places the navigation in the center of the window
	function RepositionNav(){
		var windowHeight = $window.height(); //get the height of the window
		var navHeight = $('#nav').height() / 2;
		var windowCenter = (windowHeight / 2);
		var newtop = windowCenter - navHeight;
		$('#nav').css({"top": newtop}); //set the new top position of the navigation list
	}

	//function that is called for every pixel the user scrolls. Determines the position of the background
	/*arguments:
		x = horizontal position of background
		windowHeight = height of the viewport
		pos = position of the scrollbar
		adjuster = adjust the position of the background
		inertia = how fast the background moves in relation to scrolling
	*/
	function newPos(x, windowHeight, pos, adjuster, inertia){
		return x + "% " + (-((windowHeight + pos) - adjuster) * inertia)  + "px";
	}

	//function to be called whenever the window is scrolled or resized
	function Move(){
		var pos = $window.scrollTop(); //position of the scrollbar

		//if the first section is in view...
		if($firstBG.hasClass("inview")){
			//call the newPos function and change the background position
			$firstBG.css({'backgroundPosition': newPos(0, windowHeight, pos, 500, 0)});
			//call the newPos function and change the second background position
			bg1.css({'backgroundPosition': newPos(50, windowHeight, pos, 400, 0.2)});
		}

		//if the second section is in view...
		if($secondBG.hasClass("inview")){
			//call the newPos function and change the background position
			$secondBG.css({'backgroundPosition': newPos(0, windowHeight, pos, 1550, 0)});
			//$secondBG.css({'backgroundPosition': newPos(50, windowHeight, pos, 1550, 0.3)});
			bg2.css({'backgroundPosition': newPos(70, windowHeight, pos, 3410, 0.2)});
			//call the newPos function and change the second background position
		}

		if ($thirdBG.hasClass("inview")){
			//call the newPos function and change the background position
			$thirdBG.css({'backgroundPosition': newPos(0, windowHeight, pos, 2550, 0)});
			//$secondBG.css({'backgroundPosition': newPos(50, windowHeight, pos, 1550, 0.3)});
			bg3.css({'backgroundPosition': newPos(50, windowHeight, pos, 5410, 0.2)});
			//call the newPos function and change the second background position
		}

		if ($fourthBG.hasClass("inview")){
			//call the newPos function and change the background position
			$fourthBG.css({'backgroundPosition': newPos(0, windowHeight, pos, 5850, 0)});
			//$secondBG.css({'backgroundPosition': newPos(50, windowHeight, pos, 1550, 0.3)});
			//call the newPos function and change the second background position
			bg4.css({'backgroundPosition': newPos(50, windowHeight, pos, 6610, 0.20)});
		}

		$('#pixels').html(pos); //display the number of pixels scrolled at the bottom of the page
	}

	RepositionNav(); //Reposition the Navigation to center it in the window when the script loads

	$window.resize(function(){ //if the user resizes the window...
		Move(); //move the background images in relation to the movement of the scrollbar
		RepositionNav(); //reposition the navigation list so it remains vertically central
	});

	$window.bind('scroll', function(){ //when the user is scrolling...
		Move(); //move the background images in relation to the movement of the scrollbar
	});

});