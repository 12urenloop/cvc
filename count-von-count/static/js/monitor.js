function createWebSocket(path) {
    var host = window.location.hostname;
    var port = window.location.port;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':' + port + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

$(document).ready(function() {
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
        $(".slow").removeClass("slow");
        $(".broken").removeClass("broken");
        $('.last-update').each(function () {
            var number = parseInt($(this).html());
            $(this).html(number + interval);
            if(number + interval > 60) {
                $(this).parents(".team").first().addClass("broken");
            } else if(number + interval > 30) {
                $(this).parents(".team").first().addClass("slow");
            }
        });
    }, interval * 1000);
});
