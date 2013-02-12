var express = require('express');

var config = require('./config'),
    state  = require('./state');

/* Initialize express, and boxxy state */
var app = express();
app.set('state', state.initialize());

/* The default body parser will parse JSON, which is what we need. */
app.use(express.bodyParser());

/* Basic authentication, should be used for all PUT requests from
 * count-von-count */
var basicAuth = express.basicAuth(function(user, password) {
    return user == config.BOXXY_USER && password == config.BOXXY_PASSWORD;
}, "Unauthorized");

app.get('/state', function(req, res) {
    res.send(app.get('state'));
});

app.put('/state', basicAuth, function(req, res) {
    console.log('PUT /state');
    state.reset(app.get('state'), req.body);
    res.send('OK');
});

app.put('/lap', basicAuth, function(req, res) {
    console.log('PUT /lap (' + req.body.team.name + ')');
    state.addLap(app.get('state'), req.body);
    res.send('OK');
});

app.listen(config.BOXXY_PORT);
