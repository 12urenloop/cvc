var http = require("http");
var url = require("url");

function start(view_register){
    function onRequest(request,response){
	//BOXXY
	//register view client
	var pathname = url.parse(request.url).pathname;

	view_register("sander");

	response.writeHead(200,{"Content-Type": "text/plain"});
	response.write("Hell world");
	response.end();
    }
    http.createServer(onRequest).listen(8888);
    console.log("Server started");
}
exports.start = start;