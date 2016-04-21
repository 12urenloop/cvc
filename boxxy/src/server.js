// Other modules
// =============

var express  = require('express'),
    fs       = require('fs'),
    http     = require('http'),
    socketIO = require('socket.io');

var config = require('./config'),
    boxxy  = require('./boxxy');

// Express, node.js initialization
// ===============================

var app    = express(),
    server = http.createServer(app),
    io     = socketIO.listen(server);

// Middleware setup
// ================

app.use(express.bodyParser());         // Parse JSON request bodies
app.use(express.static('public'));  // Serve the public dir as-is

// Boxxy initialization, broadcasting                                          *
// ==================================

var boxxyState = boxxy.initialize();

io.sockets.on('connection', function(socket) {
    if (boxxyState.frozen) {
        state = {};
        state.circuitLength = boxxyState.circuitLength;
        state.startTime = boxxyState.startTime;
        state.stopTime = boxxyState.stopTime;
        state.stations = boxxyState.stations;
        state.laps = boxxyState.frozenLaps;
        state.teams = boxxyState.frozenTeams;
        state.frozen = true;
        socket.emit('/state', state);
    } else {
        socket.emit('/state', boxxyState);
    }
});

boxxyState.onPutState = function(stateDelta) {
    if (!boxxyState.frozen) {
        io.sockets.emit('/state', stateDelta);
    }
}

boxxyState.onAddLap = function(lap) {
    if (!boxxyState.frozen) {
        io.sockets.emit('/lap', lap);
    }
}

boxxyState.onUpdatePosition = function(position) {
    if (!boxxyState.frozen) {
        io.sockets.emit('/position', position);
    }
}

// count-von-count facing API
// ==========================

var basicAuth = express.basicAuth(function(user, password) {
    return user == config.BOXXY_USER && password == config.BOXXY_PASSWORD;
}, "Unauthorized");

// This one isn't currently used AFAIK but it's useful for quick testing
app.get('/state', function(req, res) {
    res.send(boxxyState);
});

// Used to quickly check if boxxy is still alive.
app.put('/ping', basicAuth, function(req, res) {
    console.log('PUT /ping');
    res.send('pong');
});

app.put('/state', basicAuth, function(req, res) {
    console.log('PUT /state');
    boxxyState.putState(req.body);
    res.send('OK');
});

app.put('/lap', basicAuth, function(req, res) {
    console.log('PUT /lap (team ' + req.body.team + ')');
    boxxyState.addLap(req.body);
    res.send('OK');
});

app.put('/position', basicAuth, function(req, res) {
    console.log('PUT /position (team ' + req.body.team + ')');
    boxxyState.updatePosition(req.body);
    res.send('OK');
});

// shared boxxy module
// ===================
//
// This is a bit hacky. We want to reuse the boxxy module. However, it's written
// in CommonJS format and this is sort of awkward for the browser. Hence, we
// edit it a little to have good scoping and assign `exports` to the `boxxy`
// variable.

app.get('/js/boxxy.js', function(req, res) {
    fs.readFile('src/boxxy.js', function(err, data) {
        res.type('application/javascript');
        res.send(
            'var boxxy = function() {\n' +
            '    var exports = {};\n' +
            data +
            '    return exports;\n' +
            '}();\n');
    });
});

// General HTML views
// ==================

app.get('/', function(req, res) {
    res.redirect('/scores');
});

// DRY
function page(route, content, locals) {
    app.get(route, function(req, res) {
        fs.readFile(content, function(err, data) {
            locals.scripts = locals.scripts || [];
            locals.content = data;
            locals.host    =
                'https://' + config.BOXXY_HOSTNAME + ':' + config.BOXXY_PORT;
            res.render('application.ejs', locals);
        });
    });
}

page('/dj-contest', 'content/dj-contest.html', {
    "title": "DJ Contest",
    "bg": "bg2"
});

page('/special-laps', 'content/special-laps.html', {
    "title": "Speciale rondes",
    "bg": "bg3"
});

page('/afterparty', 'content/afterparty.html', {
    "title": "Afterparty",
    "bg": "bg5"
});

page('/scores', 'content/scores.html', {
    "title": "Live Scores",
    "scripts": [
        "/js/boxxy.js",
        "/js/jquery-1.7.1.min.js",
        "/socket.io/socket.io.js"
    ],
    "bg": "bg4"
});

page('/timeline', 'content/timeline.html', {
    "title": "Timeline",
    "scripts": [
      "/js/jquery-1.7.1.min.js",
      "/js/timeline.js"
    ],
    "container": "timeline",
    "bg": "bg1"
});

server.listen(config.BOXXY_PORT);
