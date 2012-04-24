var http = require('http'),
    Faye = require('faye')

var bayeux, server, client

var timer, currentLoop, currentItem
var currentIndex = -1;


// BEGIN TESTDATA
var items = {};
var loops = {};

items.item1 = { tag: 'url',
              url: './a.html' };
items.item2 = { tag: 'html',
              html: '<div style="background-color:red;">HELLO</div>' };
items.item3 = { tag: 'url',
              url: './b.html' };

loops.loop1 = [ { item: items.item1,
                duration: 5 },
              { item: items.item2,
                duration: 5 },
              { item: items.item3,
                duration: 5 } ];

currentLoop = 'loop1';
// END TESTDATA

var run = function(port) {
    bayeux = new Faye.NodeAdapter({ mount: '/faye' });

    server = http.createServer(function(request, response) {
        response.writeHead(200, {'Content-Type': 'text/plain'});
        response.write('Please go away :(');
        response.end();
    });

    client = bayeux.getClient();
    client.subscribe('/whatson', whatsonHandler);

    bayeux.attach(server);
    server.listen(port);

    console.log('Started listening on port ' + port);
}

var restart = function() {
    clearTimeout(timer);
    currentIndex = -1;
    publishLoop();
}

var publishLoop = function() {
    if (currentLoop !== undefined && loops[currentLoop].length > 0) {
        currentIndex = (currentIndex + 1) % loops[currentLoop].length;
        currentItem = loops[currentLoop][currentIndex].item;
        var duration = loops[currentLoop][currentIndex].duration;

        client.publish('/publications', currentItem);
        console.log('Published ' + JSON.stringify(currentItem) + ' for ' + duration + ' seconds.');
        timer = setTimeout(function() { publishLoop() }, duration * 1000);
    }
}

var whatsonHandler = function(msg) {
    if (currentLoop !== undefined && loops[currentLoop].length > 0) {
        client.publish('/publications', currentItem);
        console.log('Whats\'on? ' + JSON.stringify(currentItem));
    }
}

run(9000);

restart();