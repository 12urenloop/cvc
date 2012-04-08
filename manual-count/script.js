var DELAY = 30; // delay to prevent accidental double taps
var BUTTONS_PER_VIEW = 7;

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

    console.log('Submitting', queue[0]);

    // TODO: show error notification to user
    var errorHandler = function() {
        console.error("Error submitting", queue[0]);
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

    $('#clearButton').click(function() {
        if(confirm('Are you sure you want to clear all local data?')) {
            storage.clear();
            window.location.reload();
        }
    });

    processQueue();
});
