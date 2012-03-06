function Boxxy() {
    var self = this

    // connection callbacks
    this.connected = function() {}
    this.disconnected = function() {}
    this.timeout = function() {}

    // new information
    this.position = function(m) {}
    this.lap = function(m) {}

    // connect
    this.connect = function() {
        var url = 'http://' + window.location.hostname + ':8080'
        this.client = new Faye.Client(url + '/boxxy')
        this.connected()

        this.client.subscribe('/laps', function(message) {
            self.lap(message)
        })
        this.client.subscribe('/position', function(message) {
            self.position(message)
        })
    }
}
