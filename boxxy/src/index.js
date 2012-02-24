var server = require("./server/server.js");
var view_client = require("./viewClients/register.js");

server.start(view_client.register);

