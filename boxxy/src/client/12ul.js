function Boxxy() {
    this.position = function() {}
    this.lap = function() {}
    this.connected = function() {}
    this.disconnected = function() {}
    
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
    
    this.scores = function(callback) {
        
    }
}