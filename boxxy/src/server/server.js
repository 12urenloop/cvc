var http = require("http");
var faye = require("faye");
var faye_server = new faye.NodeAdapter({mount: '/boxxy',timeout: 45});

var url = require("url");
var t_eventParser;
var t_parseEvent;
var t_handler;

function start(eventParser,handler){
    function onRequest(request,response){
	//BOXXY
	//register view client
	var pathname = url.parse(request.url).pathname;


	response.writeHead(200,{"Content-Type": "text/plain"});
	response.write(eventParser(pathname,handler));		     
	response.end();
    }
    var server=http.createServer(onRequest).listen(8888);
    faye_server.attach(server);
    console.log("Server started");
}
console.log("server started at port 8888");
exports.start = start;
