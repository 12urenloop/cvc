var draw = {
    ctx: undefined,
    logo: undefined,
    shape: [
        {x: 0, y: 0.07},
        {x: 0.1, y: 0},
        {x: 0.9, y: 0},
        {x: 1, y: 0.07},
        {x: 1, y: 0.28},
        {x: 0.9, y: 0.35},
        {x: 0.1, y: 0.35},
        {x: 0, y: 0.28}
    ],
    scale: 850,
    circuitTop: 0.15,
    circuitHeight: 0.35,
    fps: 30,
    boxxy: undefined,
    interpolation: undefined,
    highlightIdx: 0,
    highlight: null,
    rankingTexts: ['eerste', 'tweede', 'derde', 'vierde', 'vijfde', 'zesde',
        'zevende', 'achtste', 'negende', 'tiende', 'elfde', 'twaalfde',
        'dertiende', 'veertiende', 'vijftiende', 'zestiende', 'zeventiende'
    ],    
    init: function(context, width, height, boxxy, interpolation) {
        draw.context = context;
        draw.width = width;
        draw.height = height;
        draw.circuitMargin = (draw.width - draw.scale) / 2;
        draw.logo = new Image();
        draw.logo.src = '../assets/logo.gif';
        draw.boxxy = boxxy;
        draw.interpolation = interpolation;
        window.requestAnimFrame = (function(callback){
            return window.requestAnimationFrame ||
            window.webkitRequestAnimationFrame ||
            window.mozRequestAnimationFrame ||
            window.oRequestAnimationFrame ||
            window.msRequestAnimationFrame ||
            function(callback){
                window.setTimeout(callback, 1000 / fps);
            };
        })();
        
        draw.highlight = boxxy.getTeams()[0];
        window.setInterval(function() {
            var teams = boxxy.getTeams();
            draw.highlightIdx = (draw.highlightIdx + 1) % teams.length;
            draw.highlight = teams[draw.highlightIdx];
        }, 7000);
    },
    
    clear: function() {
//        draw.context.save();
        draw.context.clearRect(0, 0, draw.width, draw.height);
//        draw.context.fillStyle = 'grey';
//        draw.context.rect(0, 0, draw.width, draw.height);
//        draw.context.fill();
//        draw.context.restore();
    },
    
    circuit: function() {
        var logoHeight = 0.35,
            logoWidth = 1.6 * logoHeight,
            logoX = (1 - logoWidth) / 2,
            logoY = 0,
            lineWidth = 0.04;
        draw.context.save();
        draw.context.translate(draw.circuitMargin, draw.circuitTop * draw.scale);
        draw.context.scale(draw.scale, draw.scale);
        draw.context.globalAlpha = 0.2;
        draw.context.drawImage(draw.logo, logoX, logoY, logoWidth, logoHeight);
        draw.context.globalAlpha = 1.0;
        draw.context.beginPath();
        draw.context.moveTo(draw.shape[0].x, draw.shape[0].y)
        for(var idx = 1; idx <= draw.shape.length; idx++) {
            var pos = draw.shape[idx % draw.shape.length];
            draw.context.lineTo(pos.x, pos.y);
        }
        draw.context.lineWidth = lineWidth;
        draw.context.strokeStyle = "#8ED6FF";
        draw.context.lineCap = 'round';
        draw.context.stroke();
        draw.context.restore();
    },
    
    countdown: function() {
        var time = draw.boxxy.countdown();
        function pad(n) {return n < 10 ? '0' + n : n;}
        draw.context.font = draw.font(20);
        draw.context.textAlign = "center";
        draw.context.textBaseline = "middle";
        draw.context.fillText(
            pad(time.h) + ":" + pad(time.m) + ":" + pad(time.s),
            draw.width / 2, (draw.circuitTop - 0.02) * draw.scale / 2
        );
    },
    
    team: function(team) {
        var teamFont = draw.font(7),
            teamRadius = 0.025;
        
        // <3 transformations
        draw.context.save();
        draw.context.translate(draw.circuitMargin, draw.circuitTop * draw.scale);
        draw.context.scale(draw.scale, draw.scale);
        
        draw.context.beginPath();
        draw.context.fillStyle = "#FF6103";
        draw.context.strokeStyle = "black";
        draw.context.lineWidth = 0.002;
        draw.context.arc(team.coords.x, team.coords.y, teamRadius, 0, Math.PI*2, true);
        draw.context.closePath();
        draw.context.fill();
        draw.context.stroke();
        
        // scale back to regular. Highly scaled text is a no no.
        // draw.context.save();
        draw.context.scale(1 / draw.scale, 1 / draw.scale);
        draw.context.font = teamFont;
        draw.context.textAlign = "center";
        draw.context.textBaseline = "middle";
        draw.context.fillStyle = "black";
        draw.context.fillText(team.id, team.coords.x * draw.scale, team.coords.y * draw.scale);
        
        if(team.highlight) {
            var teamNameX = draw.scale / 2,
                teamNameY = 0.095 * draw.scale,
                teamNameFont = draw.font(14),
                teamStatsFont = draw.font(12),
                highlightRadius = 0.057;

            draw.context.textBaseline = "middle";
            draw.context.textAlign = "center";
            draw.context.font = teamNameFont;
            draw.context.fillText(team.name, teamNameX, teamNameY);
            draw.context.font = teamStatsFont;
            var meters = team.info.laps * draw.boxxy.circuitLength + team.info.station.position - 50,
                speed = meters / (draw.boxxy.timeSinceStart() / 1000);
            draw.context.fillText(team.info.laps + ' rondjes aan ' + (speed * 3.6).toFixed(1) + ' km/u', teamNameX, teamNameY + 0.075 * draw.scale);
            draw.context.fillText(draw.rankingTexts[team.info.ranking - 1] + " plaats", teamNameX, teamNameY + 0.15 * draw.scale);
            draw.context.scale(draw.scale, draw.scale);
            draw.context.beginPath();
            draw.context.fillStyle = "#FF6103";
            draw.context.lineWidth = 0.005;
            draw.context.arc(team.coords.x, team.coords.y, highlightRadius, 0, Math.PI*2, true); 
            draw.context.closePath();
            draw.context.fill();
            draw.context.stroke();
            
            draw.context.scale(1 / draw.scale, 1 / draw.scale);
            
            draw.context.font = draw.font(17);
            draw.context.textAlign = "center";
            draw.context.textBaseline = "middle";
            draw.context.fillStyle = "black";
            
            draw.context.fillText(team.id, team.coords.x * draw.scale, team.coords.y * draw.scale);
        }
        
        draw.context.restore();
    },
    
    fact: function(fact) {
        draw.context.textBaseline = "middle";
        draw.context.textAlign = "center";
        draw.context.font = draw.font(12);
        draw.context.fillText(fact, draw.width / 2, 0.60 * draw.scale);
    },
    
    frame: function() {
        draw.clear()

        // draw everything!
        draw.circuit();
        draw.countdown();
        
        if(draw.boxxy.initialized) {
            var teams = draw.boxxy.getTeams();

            for(var idx in teams) {
                var coords = draw.interpolation.getCoords(teams[idx].id, shapes.real);
                draw.team({
                    name: teams[idx].name,
                    id: teams[idx].name[0],
                    coords: coords,
                    info: teams[idx],
                    highlight: false
                });
            }

            var coords = draw.interpolation.getCoords(draw.highlight.id, shapes.real);
            draw.team({
                name: draw.highlight.name,
                id: draw.highlight.name[0],
                coords: coords,
                info: draw.highlight,
                highlight: true
            })
            draw.fact(facts.current);
        }
        
        requestAnimFrame(function(){
            draw.frame();
        });
    },
    
    start: function() {
        requestAnimFrame(function(){
            draw.frame();
        });
    },
    
    center: function(width) {
        return (draw.width - width) / 2;
    },
    
    font: function(size) {
        return (size * draw.scale / 250).toFixed(0) + "pt Verdana";
    }
}

