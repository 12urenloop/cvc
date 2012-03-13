function Boxxy() {
    var self = this;
    this.teams = {};
    this.ranking = [];
    this.stations = {};
    // log everything to console
    this.debug = true;

    // information hooks
    this.receivedConfig = function(c) {}
    this.updatedPosition = function(m) {}
    this.addedLap = function(m) {}

    // connect
    this.connect = function(domain) {
        var url = 'http://' + (domain || window.location.hostname) + ':8080',
            http = null;

        if(window.XMLHttpRequest) http = new XMLHttpRequest();
        else http = new ActiveXObject('Microsoft.XMLHTTP');

        // Get config, uncached
        http.open('GET', url + '/init?t=' + new Date().getTime());
        http.onreadystatechange = function() {
            if(http.readyState != 4 || http.status != 200) return;

            config = eval('(' + http.responseText + ')');
            if(self.debug) console.log("[CONFIG] " + JSON.stringify(config))
            self.teams = config.teams;
            
            for(var stationIdx = 0; stationIdx < config.stations.length; stationIdx++) {
                var station = config.stations[stationIdx];
                var nextStation = config.stations[(stationIdx + 1) % config.stations.length];
                station.next = nextStation;
                self.stations[station.name] = station;
            }

            // Store the order of each team
            for(var idx in self.teams) {
                self.ranking.push(self.teams[idx]);
            }
            updateRanking();

            self.receivedConfig(config);
        }
        http.send(null);

        // Setup websocket
        self.client = new Faye.Client(url + '/boxxy');
        self.client.subscribe('/laps', function(message) {
            if(self.debug) console.log("[LAP] " + JSON.stringify(message))
            var team = self.teams[message.team.id];

            // Update ranking info
            team.laps = message.team.laps;
            updateRanking();

            // Replace with more detailed team info
            message.team = team;
            self.addedLap(message);
        });
        self.client.subscribe('/position', function(message) {
            if(self.debug) console.log("[POSITION] " + JSON.stringify(message));
            self.updatedPosition(message);
        });
    }

    this.getTeams = function() {
        return self.ranking;
    }
    
    this.getStations = function() {
        return self.stations;
    }

    // Utility methods
    var updateRanking = function() {
        self.ranking.sort(function(a, b) { return b.laps - a.laps; });
        for (var i = self.ranking.length - 1; i >= 0; i--) {
            self.ranking[i].ranking = i + 1;
        };
    }
    
    
}
