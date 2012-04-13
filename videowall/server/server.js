var http = require('http'),
    Faye = require('faye')

var bayeux, server, client

var timer

var loops = {}
var currentIndex = -1
var currentItem;
var currentLoop;

var NO_ITEM_TIMEOUT = 5000;

/*
var loopA = [ {url: 'http://www.reddit.com', duration: 30}
            , {url: 'http://news.ycombinator.com', duration: 60}
            ];
loops["loopA"] = loopA;

var loopB = [ {url: 'http://zeus.ugent.be', duration: 15}
            , {url: 'http://buienradar.be', duration: 15}
            , {url: 'http://deredactie.be', duration: 15}
            , {url: 'http://destandaard.be', duration: 15}
            ];
loops["loopB"] = loopB;

currentLoop = "loopA";
*/

var run = function(port) {
    bayeux = new Faye.NodeAdapter({ mount: '/faye' });

    server = http.createServer(function(request, response) {
        response.writeHead(200, {'Content-Type': 'text/plain'});
        response.write('Please go away :(');
        response.end();
    });

    client = bayeux.getClient();
    client.subscribe('/loops', loopsHandler);
    client.subscribe('/loops/save', saveLoopHandler);
    client.subscribe('/loops/delete', deleteLoopHandler);
    client.subscribe('/loops/play', playLoopHandler);
    client.subscribe('/whatson', whatsonHandler);

    bayeux.attach(server);
    server.listen(port);

    console.log('Started listening on port ' + port);
}

var saveLoopHandler = function(msg) {
    console.log('Saving loop ' + msg.loop);
    loops[msg.loop] = msg.items;
    client.publish('/loops/updates', { loops: loops, currentLoop: currentLoop });
}

var deleteLoopHandler = function(msg) {
    console.log('Deleting loop ' + msg.loop);
    delete loops[msg.loop];
    client.publish('/loops/updates', { loops: loops, currentLoop: currentLoop });
    restart();
}

var playLoopHandler = function(msg) {
    if (currentLoop !== msg.loop && msg.loop in loops) {
        console.log('Playing loop ' + msg.loop);
        currentLoop = msg.loop;
        client.publish('/loops/updates', { loops: loops, currentLoop: currentLoop });
        restart();
    }
}

var loopsHandler = function(msg) {
    client.publish('/loops/updates', { loops: loops, currentLoop: currentLoop });
}

var restart = function() {
    currentIndex = -1;
    clearTimeout(timer);
    publishLoop();
}

var publishLoop = function() {
    if (currentLoop !== undefined && loops[currentLoop].length > 0) {
        currentIndex = (currentIndex + 1) % loops[currentLoop].length;
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

run(8000);

restart();