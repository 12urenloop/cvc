var http = require("http");
var url = require("url");

function start(eventParser,handler){
    function onRequest(request,response){
	//BOXXY
	//register view client
	var pathname = url.parse(request.url).pathname;
	eventParser(pathname,handler);

	response.writeHead(200,{"Content-Type": "text/plain"});
	response.write("Hell world");
	response.end();
    }
    http.createServer(onRequest).listen(8888);
    console.log("Server started");
}
exports.start = start;