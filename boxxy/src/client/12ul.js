function Boxxy() {
    var self = this
    this.host = (window.config && window.config.host) || 'live.12urenloop.be'
    this.port = (window.config && window.config.port) || 8080
    this.url = 'http://' + this.host + ':' + this.port

    // include Faye
    var script = document.createElement('script')
    script.src = this.url + '/boxxy.js'
    script.type = 'text/javascript'
    script.onload = function() {
        self.connect()
    }
    script.onreadystatechange = script.onload
    document.head.appendChild(script)

    // standard empty callbacks
    this._position = function(m) {}
    this._lap = function(m) {}
    this._connected = function(m) {}
    this._disconnected = function(m) {}
    this._timeout = function(m) {}
    
    this.position = function(callback) {
        this._position = callback
    }
    
    this.lap = function(callback) {
        this._lap = callback
    }
    
    this.disconnected = function(callback) {
        this._disconnected = callback
    }
    
    this.connected = function(callback) {
        this._connected = callback
    }
    
    this.timeout = function(callback) {
        this._timeout = callback
    }
    
    this.scores = function(callback) {
        
    }
    
    this.connect = function() {
        this.client = new Faye.Client(this.url + '/boxxy')
        this._connected()

        this.client.subscribe('/laps', function(message) {
            self._lap(message)
        })
        this.client.subscribe('/position', function(message) {
            self._position(message)
        })
    }
}