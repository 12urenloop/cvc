function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8000' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

var handlers = {
    "lap": function(event) {
        $('[data-team-id = "' + event.team.id + '"]')
            .children('.laps')
            .html(event.team.laps + 1);
    }
};

$(document).ready(function() {
    var ws = createWebSocket('/monitor/subscribe');
    ws.onmessage = function(event) {
        try {
            var json = JSON.parse(event.data);
            var handler = handlers[json.type];
            if(handler) handler(json);
        } catch(err) {
            alert(err);
        }
    };
});
