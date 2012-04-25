var facts = {
    weatherUrl: 'http://free.worldweatheronline.com/feed/weather.ashx',
    weather: null,
    boxxy: null,
    factIdx: -1,
    init: function(boxxy) {
        facts.boxxy = boxxy;
        facts.updateWeather();
        facts.next();
        setInterval(facts.next, 5000);
        setInterval(facts.updateWeather, 60000);
    },
    
    lap: function(message) {
        
    },
    
    position: function(message) {
        
    },
    
    list: [
        function() {return "Welkom op de 29e 12 Urenloop!";},
        function() {
            var temp = parseInt(facts.weather.temp_C);
            return "Het is momenteel " + temp + " graden Celsius";
        },
        function() {
            var direction = facts.weather.winddir16Point,
                speed = parseInt(facts.weather.windspeedKmph);
            direction = direction.replace(/S/g, "Z").replace(/E/g, "O");
            
            return "Wind: " + speed.toFixed(0) + " km/u " + direction;
        },
        function() {
            var teams = facts.boxxy.getTeams(),
                totalLaps = 0;
            for(var idx in teams) {
                totalLaps += teams[idx].laps;
            }
            var distance = totalLaps * facts.boxxy.circuitLength / 1000;
            
            return "Er is al " + distance.toFixed(1) + " km afgelegd.";
        },
        function() {
            return "Zeus WPI is de max.";
        }
    ],
    
    updateWeather: function() {
        $.ajax({
            url: facts.weatherUrl,
            success:function(data) {
                facts.weather = data.data.current_condition[0];
                console.log(data)
            },
            dataType: 'jsonp',
            data: {
                q: 'Ghent,Belgium',
                format: 'json',
                num_of_days: 0,
                key: 'fb534d2298202112122004'
            }
        });
    },
    
    next: function() {
        facts.factIdx = (facts.factIdx + 1) % facts.list.length;
        facts.current = facts.list[facts.factIdx]();
    }
};