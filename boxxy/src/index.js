var server = require("./server/server.js");
var requestHandler = require("./requestHandlers/requestHandler.js");
var eventParser = require("./requestHandlers/eventParser.js");

var handler = {};
handler["/"] = requestHandler.default_handler;
handler["/remove"] = requestHandler.remove_view_client;
handler["/add"]   = requestHandler.add_view_client;

server.start(eventParser.parseEvent,handler);

