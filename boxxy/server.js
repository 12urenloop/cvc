var http = require("http");
function onRequest(request, respone){
    //subscribe new view to API
    //list API-calls
    respone.writeHead(200,{"Content-Type": "text/plain"});
    reponse.write("BOXXY");
    reponse.end();
};

http.createServer(onRequest).listen(8888);
