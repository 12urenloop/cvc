var express = require('express');

var config = require('./config');

var app = express();

app.get('/', function(req, res) {
    res.send('Hello world!');
});

app.listen(config.BOXXY_PORT);
