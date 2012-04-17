function clearCanvas(){
    var canvas = document.getElementById("boxxy-interpolation");
    var context = canvas.getContext("2d");
    context.clearRect(0, 0, canvas.width, canvas.height);
}

function drawTeam(name, position) {
    var canvas = document.getElementById("boxxy-interpolation");
    var context = canvas.getContext("2d");
    context.font = "18pt Verdana";
    context.textAlign = "center";
    context.fillStyle = "black";
    context.fillText(name[0], 50 + position.x * 300, 50 + position.y * 300);
}

function drawStation(id, position) {
    var canvas = document.getElementById("boxxy-interpolation");
    var context = canvas.getContext("2d");
    context.font = "14pt Verdana";
    context.textAlign = "center";
    context.fillStyle = "red";
    context.fillText("" + id, 50 + position.x * 300, 50 + position.y * 300);
}

function drawCircuit() {
    var canvas = document.getElementById("boxxy-interpolation");
    var context = canvas.getContext("2d");
    context.beginPath();
    context.rect(50, 50, 300, 300 / 1.62803);
    context.lineWidth = 10;
    context.strokeStyle = "#8ED6FF";
    context.stroke();
}

function drawRealCircuit() {
    var canvas = document.getElementById("boxxy-interpolation"),
        context = canvas.getContext("2d"),
        scale = 300,
        offsetX = 50,
        offsetY = 50,
        shape = [
            {x: 0, y: 0.07},
            {x: 0.1, y: 0},
            {x: 0.9, y: 0},
            {x: 1, y: 0.07},
            {x: 1, y: 0.28},
            {x: 0.9, y: 0.35},
            {x: 0.1, y: 0.35},
            {x: 0, y: 0.28}
        ];
    context.beginPath();
    context.moveTo(shape[0].x * scale + offsetX, shape[0].y * scale + offsetY)
    for(var idx = 1; idx <= shape.length; idx++) {
        var pos = shape[idx % shape.length];
        context.lineTo(pos.x * scale + offsetX, pos.y * scale + offsetY);
    }
    context.lineWidth = 10;
    context.strokeStyle = "#8ED6FF";
    context.lineCap = 'round';
    context.stroke();
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