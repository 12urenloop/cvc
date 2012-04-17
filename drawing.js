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
    padding: 17,
    offsetY: 55,
    fps: 30,
    boxxy: undefined,
    interpolation: undefined,
    
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
    },
    
    clear: function() {
        draw.context.clearRect(0, 0, canvas.width, canvas.height);
    },
    
    circuit: function() {
        draw.context.drawImage(draw.logo, draw.padding, draw.offsetY, 250, 88);
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
    
    frame: function() {
        draw.clear()

        // draw everything!
        draw.circuit();

        var teams = boxxy.getTeams();
        for(var idx in teams) {
            var coords = draw.interpolation.getCoords(teams[idx].id, shapes.real);
            draw.team(teams[idx].name, coords);
        }

        requestAnimFrame(function(){
            draw.frame();
        });
    },
    
    start: draw.frame
}

function drawTeam(context, name, position) {
    context.font = "18pt Verdana";
    context.textAlign = "center";
    context.fillStyle = "black";
    context.fillText(name[0], 50 + position.x * 300, 50 + position.y * 300);
}

function drawStation(context, id, position) {
    context.font = "14pt Verdana";
    context.textAlign = "center";
    context.fillStyle = "red";
    context.fillText("" + id, 50 + position.x * 300, 50 + position.y * 300);
}

function initCanvas(boxxy, interpolation) {
    var fps = 30;

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

    function frame() {
        clearCanvas()

        // draw everything!
        drawRealCircuit();

        var stations = boxxy.getStations();
        for(var name in stations) {
            var station = stations[name];
            var pos = shapes.real(station.position / boxxy.circuitLength);
            drawStation(station.name[station.name.length - 1], pos);
        }
        var teams = boxxy.getTeams();
        for(var idx in teams) {
            var coords = interpolation.getCoords(teams[idx].id, shapes.real);
            drawTeam(teams[idx].name, coords);
        }

        requestAnimFrame(function(){
            frame();
        });
    }
    frame();
}