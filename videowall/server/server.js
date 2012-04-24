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

loops.loop1 = [ { item: "item1",
                duration: 5 },
              { item: "item2",
                duration: 5 },
              { item: "item3",
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
    client.subscribe('/status', statusHandler);
    client.subscribe('/loops/save', saveLoopsHandler);
    client.subscribe('/loops/play', playLoopHandler);
    client.subscribe('/loops/delete', deleteLoopHandler);
    client.subscribe('/items/save', saveItemsHandler);
    client.subscribe('/items/play', playItemHandler);
    client.subscribe('/items/delete', deleteItemHandler);

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
        currentItem = items[loops[currentLoop][currentIndex].item];
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
var statusHandler = function(msg) {
    pushUpdate();
}

var saveLoopsHandler = function(msg) {
    console.log('Save loop request received: ' + JSON.stringify(msg));
    loops[msg.loop] = msg.items;

    pushUpdate();
}

var playLoopHandler = function(msg) {
    console.log('Play loop request received: ' +JSON.stringify(msg));
    currentLoop = msg.loopName;
    restart();
}

var deleteLoopHandler = function(msg) {
    console.log('Delete loop request received: ' + JSON.stringify(msg));
    delete loops[msg.loopName];

    pushUpdate();
}

var saveItemsHandler = function(msg) {
    console.log('Save item request received: ' + JSON.stringify(msg));
    items[msg.itemName] = msg.item;

    pushUpdate();
}

var playItemHandler = function(msg) {
    console.log('Play item request received: ' + JSON.stringify(msg));

    clearTimeout(timer);
    currentItem = items[msg.itemName];
    currentLoop = undefined;
    currentIndex = -1;
    console.log(JSON.stringify(currentItem));

    client.publish('/publications', currentItem);
    console.log('Published ' + JSON.stringify(currentItem) + ' for EVER!');
    pushUpdate();
}

var deleteItemHandler = function(msg) {
    console.log('Delete item request received: ' + JSON.stringify(msg));
    delete items[msg.itemName];

    pushUpdate();
}

var pushUpdate = function() {
    client.publish('/updates', { loops:loops, items:items, currentLoop:currentLoop });
    console.log('Update pushed!');
}

run(9000);

restart();