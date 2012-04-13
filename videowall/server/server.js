var http = require('http'),
    Faye = require('faye'),
    bayeux = new Faye.NodeAdapter({mount: '/faye'});

var server = http.createServer(function(request, response) {
    response.writeHead(200, {'Content-Type': 'text/plain'});
    response.write('Hello, non-Bayeux request');
    response.end();
});

bayeux.attach(server);

server.listen(8000);

var client = bayeux.getClient();

client.subscribe('/whatson', whatsonHandler);
function whatsonHandler(msg) {
    client.publish('/publications', currentItem);
    console.log('Whats\'on? ' + currentItem.url + ' for ' + currentItem.duration + ' seconds.');
};

var hackernews = {
    url: './a.html',
    duration: 10
};

var redditAll = {
    url: './b.html',
    duration: 5
};

var redditPics = {
    url: './c.html',
    duration: 20
}

var queue = [hackernews, redditAll, redditPics];
var currentItem;

function publishLoop() {
    publishItem();
};
function publishItem() {
    currentItem = queue.shift();
    queue.push(currentItem);

    client.publish('/publications', currentItem);
    console.log('Published ' + currentItem.url + ' for ' + currentItem.duration + ' seconds.');

    setTimeout(publishLoop, currentItem.duration * 1000);
};
publishLoop();