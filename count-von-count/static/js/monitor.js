function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8000' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

function teamDiv(id) {
    return $('[data-team-id = "' + id + '"]');
}

var speedData = {};
var config = {};

function plotSpeedData(id) {
    $.plot(teamDiv(id).children('.speed'), [speedData[id]], {
        xaxis: {
            min: 0,
            max: config.circuitLength
        },
        yaxis: {
            min: 0
        }
    });
}

var handlers = {
    lap: function(event) {
        var id = event.team.id;
        teamDiv(id).children('.laps').html(event.team.laps + 1);

        var last = speedData[id][speedData[id].length - 1];
        speedData[id] = [[last[0] - config.circuitLength, last[1]]];
    },

    progression: function(event) {
        var id = event.team.id;
        var position = event.station.position;
        if(position < speedData[id][speedData[id].length - 1][0]) {
            position += config.circuitLength;
        }

        speedData[id].push([position, event.speed]);
        plotSpeedData(id);
    }
};

$(document).ready(function() {
    $.get('/config.json', function(data) {
        config = data;

        $('.team').each(function() {
            var id = $(this).attr('data-team-id');
            speedData[id] = [[0, 0]];
            plotSpeedData(id);
        });

        var ws = createWebSocket('/monitor/feed');
        ws.onmessage = function(event) {
            try {
                var json = JSON.parse(event.data);

                if(json.selector && json.html) {
                    partial(json);
                } else {
                    var handler = handlers[json.type];
                    if(handler) handler(json);
                }
            } catch(err) {
                alert(err);
            }
        };
    });
});
