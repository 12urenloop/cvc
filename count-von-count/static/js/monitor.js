function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8000' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

$(document).ready(function() {
    var ws = createWebSocket('/monitor/subscribe');
    ws.onmessage = function(event) {
        alert(event.data);
    };
});
