var express  = require('express'),
    http     = require('http'),
    socketIO = require('socket.io');

var config = require('./config'),
    boxxy  = require('./boxxy');

/* Initialize express and sockets.io */
var app    = express();
var server = http.createServer(app);
var io     = socketIO.listen(server);

/* Initialize boxxy state for broadcasting */
var boxxyState = boxxy.initialize();
boxxyState.onPutState = function(state) {
    io.sockets.emit('state', state);
}

boxxyState.onAddLap = function(lap) {
    io.sockets.emit('lap', lap);
}

/* The default body parser will parse JSON, which is what we need. */
app.use(express.bodyParser());

/* Basic authentication, should be used for all PUT requests from
 * count-von-count */
var basicAuth = express.basicAuth(function(user, password) {
    return user == config.BOXXY_USER && password == config.BOXXY_PASSWORD;
}, "Unauthorized");

app.get('/state', function(req, res) {
    res.send(boxxyState);
});

app.put('/state', basicAuth, function(req, res) {
    console.log('PUT /state');
    boxxyState.putState(req.body);
    res.send('OK');
});

app.put('/lap', basicAuth, function(req, res) {
    console.log('PUT /lap (' + req.body.team.name + ')');
    boxxyState.addLap(req.body);
    res.send('OK');
});

/* When a new client connects, send the entire state */
io.sockets.on('connection', function(socket) {
    socket.emit('state', boxxyState);
});

server.listen(config.BOXXY_PORT);
