var http = require("http");

function start(){
    function onRequest(request,response){
	//BOXXY
	//register view client
	response.writeHead(200,{"Content-Type": "text/plain"});
	response.write("Hell world");
	response.end();
    }
    http.createServer(onRequest).listen(8888);
    console.log("Server started");
}
exports.start = start;