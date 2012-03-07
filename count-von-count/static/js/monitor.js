function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8000' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

$(document).ready(function() {
    $.get('/config.json', function(data) {
        config = data;

        var ws = createWebSocket('/monitor/feed');
        ws.onmessage = function(event) {
            try {
                var json = JSON.parse(event.data);
                partial(json);
            } catch(err) {
                alert(err);
            }
        };

        var interval = 5;
        setInterval(function () {
            $('.last-update').each(function () {
                var number = parseInt($(this).html());
                $(this).html(number + interval);
            });
        }, interval * 1000);
    });
});
