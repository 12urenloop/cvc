var DELAY = 30; // delay to prevent accidental double taps
var BUTTONS_PER_VIEW = 7;
var error = '';

// Detect local storage
var storage;
try {
  var uid = new Date;
  (storage = window.localStorage).setItem(uid, uid);
  var fail = storage.getItem(uid) != uid;
  storage.removeItem(uid);
  fail && (storage = false);
} catch(e) {}

if(!storage) {
    alert('Your browser does not support HTML5-localStorage.');
}

function initStorage() {
    var teams = window.teams || false;
    if(!teams) return;

    for(var teamId in teams) {
        if(!storage.getItem(teamId)) {
            storage.setItem(teamId, JSON.stringify(teams[teamId]));
        }
    }
    storage.setItem('teams', JSON.stringify(Object.keys(teams)));
}

function createView(viewNumber) {
    var teams = JSON.parse(storage.getItem('teams')),
        base = viewNumber * BUTTONS_PER_VIEW,
        container = $('#buttonHolder');

    container.empty();
    for (var i = base; i < teams.length && i < base + BUTTONS_PER_VIEW; i++) {
        (function(teamName) {
            var team = JSON.parse(storage.getItem(teamName));

            var button = $('<li id="' + teams[i] + '">');
            button.append('<h2>' + team.id + '<span>' + team.name + '</span></h2>');
            button.append('<p class="laps">' + team.laps + ' rondjes</p>');
            container.append(button);

            button.click(function() {
                var b = $(this);
                if(b.hasClass('disabled')) return;

                b.addClass('disabled');
                setTimeout(function() {
                    b.removeClass('disabled');
                }, DELAY * 1000);

                addLap(teamName);
            });
        })(teams[i]);
    };
}

function overlay() {
	el = document.getElementById("overlay");
	el.style.visibility = (el.style.visibility == "visible") ? "hidden" : "visible";
	return false;
}

function createAdminView(){
	overlay();
	toggleAdminNotification(false);
	
	var errorContainer = $('#overlayError');
	errorContainer.empty();
	errorContainer.append($('<span>' + error + '</span>'));

	var queueContainer = $('#overlayQueue');
	queueContainer.empty();
	var teams = JSON.parse(storage.getItem('teams'))
	var queue = JSON.parse(storage.getItem('requestQueue') || "[]");
	var queueListing = $('<table id="queueTable"><tr><th>ID</th><th>Teamnaam</th><th>Tijd</th></tr>');
	for(var i = 0; i < queue.length; i++){
		var row = $('<tr>');
		row.append('<td>' + queue[i].team + '</td>');
		var teamId = 'team-' + queue[i].team;
		row.append('<td>' + JSON.parse(storage.getItem(teamId)).name + '</td>');
		var time = new Date(queue[i].time * 1000);
		row.append('<td>' + ('0'+time.getHours()).slice(-2) + ':' + ('0'+time.getMinutes()).slice(-2) + ':' + ('0'+time.getSeconds()).slice(-2) + '</td>');
		queueListing.append(row);
	}
	queueContainer.append(queueListing);
}

function toggleAdminNotification(unread){
	var color = "#FFF";
	if(unread) {
		color = "#FF0000";
	} else {
		color = "#FFF";
	}
	document.getElementById("adminButton").style.backgroundColor = color;
}

function addLap(teamId) {
    var team = JSON.parse(storage.getItem(teamId));
    if(!team) return;

    team.laps = parseInt(team.laps) + 1;
    console.log(team);
    $('#' + teamId + ' .laps').text(team.laps + ' rondjes');

    var queue = JSON.parse(storage.getItem('requestQueue') || "[]");
    queue.push({team: team.id, time: Math.floor(new Date().getTime() / 1000)});

    storage.setItem('requestQueue', JSON.stringify(queue));
    storage.setItem(teamId, JSON.stringify(team));
	
    if(queue.length > 1) return;
    else processQueue();
}

function processQueue() {
    var queue = JSON.parse(storage.getItem('requestQueue') || "[]");
    if(queue.length == 0) return;

    console.log("Submitting " + JSON.stringify(queue[0]));

    var errorHandler = function(jqXHR, textStatus, errorThrown) {
        console.error("Error submitting " + JSON.stringify(queue[0]));
		error = textStatus + " when submitting " + JSON.stringify(queue[0]) + ":<BR/>";
		error += errorThrown + " " + jqXHR.status + " (" + jqXHR.statusText + ") ";
	if (document.getElementById("overlay").style.visibility != "visible") toggleAdminNotification(true);
        setTimeout(processQueue, 1000);
    }

    $.ajax('submit.php', {
        type: 'POST',
        data: queue[0],
        error: errorHandler,
        success: function(data) {
            if(parseInt(data) != 1) errorHandler();
            else {
                var queue = JSON.parse(storage.getItem('requestQueue') || "[]");
                queue.shift();
                storage.setItem('requestQueue', JSON.stringify(queue));
                if(queue.length > 0) processQueue();
            }
        }
    });
}

$(function() {
    initStorage();

    currentView = 0;
    createView(currentView);
    $('#viewSwitcher').click(function() {
        currentView = currentView == 0 ? 1 : 0;
        createView(currentView);
        $(this).text('iPad ' + (currentView + 1) + '/2');
    });

	$('#adminButton').click(function() {
        createAdminView();
    });

    $('#clearButton').click(function() {
        if(confirm('Are you sure you want to clear all local data?')) {
            storage.clear();
            window.location.reload();
        }
    });

	$('#closeModal').click(function() {
        overlay();
    });

    processQueue();
});
