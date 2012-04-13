var http = require('http'),
    Faye = require('faye')

var bayeux, server, client
var currentLoop;
var currentIndex = -1;
var loops = {}

var NO_ITEM_TIMEOUT = 5000

var timer

var run = function(port) {
    bayeux = new Faye.NodeAdapter({mount: '/faye'});

    server = http.createServer(function(request, response) {
        response.writeHead(200, {'Content-Type': 'text/plain'});
        response.write('Please go away :(');
        response.end();
    });

    bayeux.attach(server);

    server.listen(port);

    console.log('Started listening on port ' + port);

    client = bayeux.getClient();
    client.subscribe('/whatson', whatsonHandler);
    client.subscribe('/config', configHandler);

    publishLoop();
}

var publishLoop = function() {
    if (currentLoop !== undefined && loops[currentLoop].length > 0) {
        currentIndex = (currentIndex + 1) % loops[currentLoop].length
        currentItem = loops[currentLoop][currentIndex];

        client.publish('/publications', currentItem);
        console.log('Published ' + currentItem.url + ' for ' + currentItem.duration + ' seconds.');

        timer = setTimeout(function() { publishLoop() }, currentItem.duration * 1000);
    }
}

var whatsonHandler = function(msg) {
    if (currentLoop !== undefined && loops[currentLoop].length > 0) {
        var currentItem = loops[currentLoop][currentIndex];
        client.publish('/publications', currentItem);
        console.log('Whats\'on? ' + currentItem.url + ' for ' + currentItem.duration + ' seconds.');
    }
}

var configHandler = function(msg) {
    if (msg.request === 'loops') {
        console.log('Config: Publishing loops');
        client.publish('/config', {response: 'loops', loops: loops, currentLoop: currentLoop});
    } else if (msg.request === 'new-loops') {
        console.log('Config: new loops: ' + JSON.stringify(msg.loops));
        loops = msg.loops;
        if (loops[currentLoop] === undefined) {
            console.log("Config: deleted current loop, stopping timer");
            clearTimeout(timer);
        }
        client.publish('/config', {response: 'loops', loops: loops, currentLoop: currentLoop});
    } else if (msg.request === 'new-currentLoop') {
        if (msg.newCurrentLoop !== currentLoop) {
            console.log('Config: loopchange: ' + msg.newCurrentLoop)
            currentLoop = msg.newCurrentLoop;
            client.publish('/config', {response: 'loops', loops: loops, currentLoop: currentLoop});
            restart();
        }
    }
}

var restart = function() {
    currentIndex = -1;
    clearTimeout(timer);
    publishLoop();
}

var loopA = [{url: './a.html', duration: 15}, {url: './b.html', duration: 10}];
loops["loopA"] = loopA;
currentLoop = "loopA";

run(8000)
