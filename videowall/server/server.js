var http = require('http'),
    Faye = require('faye')

var bayeux, server, client
var queue, currentItem, publishedAt

var run = function(port) {
    bayeux = new Faye.NodeAdapter({mount: '/faye'});

    server = http.createServer(function(request, response) {
        response.writeHead(200, {'Content-Type': 'text/plain'});
        response.write('Please go away :(');
        response.end();
    });

    bayeux.attach(server);

    server.listen(8000);

    client = bayeux.getClient();
    client.subscribe('/whatson', whatsonHandler);

    publishLoop();
}

var publishLoop = function() {
    currentItem = queue.shift();
    queue.push(currentItem);

    client.publish('/publications', currentItem);
    console.log('Published ' + currentItem.url + ' for ' + currentItem.duration + ' seconds.');

    setTimeout(function() { publishLoop() }, currentItem.duration * 1000);
}

var whatsonHandler = function(msg) {
    client.publish('/publications', currentItem);
    console.log('Whats\'on? ' + currentItem.url + ' for ' + currentItem.duration + ' seconds.');
}

/** Random data */
var siteA = {
    url: './a.html',
    duration: 10
};

var siteB = {
    url: './b.html',
    duration: 5
};

var siteC = {
    url: './c.html',
    duration: 20
}

queue = [siteA, siteB, siteC];

run()
