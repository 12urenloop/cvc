function Boxxy() {
    var self = this,
        state = {};

    // information hooks
    this.receivedConfig = function(c) {}
    this.updatedPosition = function(m) {}
    this.addedLap = function(m) {}

    // connect
    this.connect = function(domain) {
        var url = 'http://' + (domain || window.location.hostname) + ':8080',
            http = null;
        console.log(url);

        if(window.XMLHttpRequest) http = new XMLHttpRequest();
        else http = new ActiveXObject('Microsoft.XMLHTTP');

        // Get config
        http.open('GET', url + '/init?t=' + new Date().getTime());
        http.onreadystatechange = function() {
            if(http.readyState != 4 || http.status != 200) return;
            self.state = eval('(' + http.responseText + ')');
            self.receivedConfig(self.state);
        }
        http.send(null);

        // Setup websocket
        self.client = new Faye.Client(url + '/boxxy');
        self.client.subscribe('/laps', function(message) {
            self.addedLap(message);
        });
        self.client.subscribe('/position', function(message) {
            self.updatedPosition(message);
        });
    }
}
