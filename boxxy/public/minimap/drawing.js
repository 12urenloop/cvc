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
    scale: 250,
    width: 288,
    height: 160,
    padding: 17,
    offsetY: 55,
    circuitHeight: 88,
    fps: 30,
    boxxy: undefined,
    interpolation: undefined,
    highlight: 0,
    
    init: function(context, boxxy, interpolation) {
        draw.context = context;
        draw.logo = new Image();
        draw.logo.src = '/assets/logo-200.png';
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
            var teams = boxxy.getTeams(),
                idx = Math.round(Math.random() * (teams.length - 1));
            draw.highlight = teams[idx];
        }, 3000);
    },
    
    clear: function() {
        draw.context.clearRect(0, 0, draw.width, draw.height);
    },
    
    circuit: function() {
        var scale = 0.4,
            logoWidth = 328 * scale,
            logoHeight = 200 * scale,
            logoX = (draw.width - logoWidth) / 2,
            logoY = draw.offsetY + (draw.circuitHeight - logoHeight) / 2;
        draw.context.globalAlpha = 0.2;
        draw.context.drawImage(draw.logo, logoX, logoY, logoWidth, logoHeight);
        draw.context.globalAlpha = 1.0;
        draw.context.beginPath();
        draw.context.moveTo(draw.shape[0].x * draw.scale + draw.padding, draw.shape[0].y * draw.scale + draw.offsetY)
        for(var idx = 1; idx <= draw.shape.length; idx++) {
            var pos = draw.shape[idx % draw.shape.length];
            draw.context.lineTo(pos.x * draw.scale + draw.padding, pos.y * draw.scale + draw.offsetY);
        }
        draw.context.lineWidth = 10;
        draw.context.strokeStyle = "#8ED6FF";
        draw.context.lineCap = 'round';
        draw.context.stroke();
    },
    
    team: function(team) {
        var teamX = draw.padding + team.coords.x * draw.scale,
            teamY = draw.offsetY + team.coords.y * draw.scale,
            teamFont = "14pt Verdana";
            
        draw.context.beginPath();
        draw.context.fillStyle = "#FF6103"
        draw.context.arc(teamX, teamY, 10, 0, Math.PI*2, true); 
        draw.context.closePath();
        draw.context.fill();
        draw.context.font = teamFont;
        draw.context.textAlign = "center";
        draw.context.textBaseline = "middle";
        draw.context.fillStyle = "black";
        draw.context.fillText(team.id, teamX, teamY);
        
        if(team.highlight) {
            var teamNameX = 0.1 * draw.scale + draw.padding,
                teamNameY = 0.07 * draw.scale + draw.offsetY,
                teamNameFont = "14pt Verdana",
                teamStatsFont = "12pt Verdana";

            draw.context.textBaseline = "top";
            draw.context.textAlign = "left";
            draw.context.font = teamNameFont;
            draw.context.fillText(team.name, teamNameX, teamNameY);
            draw.context.font = teamStatsFont;
            draw.context.fillText('rondjes: ' + team.info.laps, teamNameX, teamNameY + 17);
            
            draw.context.beginPath();
            draw.context.fillStyle = "#FF6103"
            draw.context.arc(teamX, teamY, 15, 0, Math.PI*2, true); 
            draw.context.closePath();
            draw.context.fill();
            
            draw.context.font = "20pt Verdana";
            draw.context.textAlign = "center";
            draw.context.textBaseline = "middle";
            draw.context.fillStyle = "black";
            
            draw.context.fillText(team.id, teamX, teamY);
        }
    },
    
    frame: function() {
        draw.clear()

        // draw everything!
        draw.circuit();

        var teams = draw.boxxy.getTeams();

        for(var idx in teams) {
            var coords = draw.interpolation.getCoords(teams[idx].id, shapes.real);
            draw.team({
                name: teams[idx].name,
                id: teams[idx].id.split('-')[1],
                coords: coords,
                info: teams[idx],
                highlight: false
            });
        }
        
        var coords = draw.interpolation.getCoords(draw.highlight.id, shapes.real);
        draw.team({
            name: draw.highlight.name,
            id: draw.highlight.id.split('-')[1],
            coords: coords,
            info: draw.highlight,
            highlight: true
        })
        
        requestAnimFrame(function(){
            draw.frame();
        });
    },
    
    start: function() {
        requestAnimFrame(function(){
            draw.frame();
        });
    }
}

