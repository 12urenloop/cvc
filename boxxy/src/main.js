var express = require('express');

var config = require('./config'),
    state  = require('./state');

/* Initialize express */
var app = express();
app.set('state', state.initialize());

/* Basic authentication, should be used for all PUT requests from
 * count-von-count */
var basicAuth = express.basicAuth(function(user, password) {
    return user == config.BOXXY_USER && password == config.BOXXY_PASSWORD;
}, "Unauthorized");

app.get('/state', function(req, res) {
    res.send(app.get('state'));
});

app.put('/state', basicAuth, function(req, res) {
    app.set(req.body);
});

app.listen(config.BOXXY_PORT);
