var faye = require('faye')

var SERVER_PASSWORD = 'tetten', // key required to publish message
    CVC_PASSWORD = 'tetten'     // shared key with count-von-count

// Checks published messages for the publishing key. Only clients with the
// publishing key can publish messages to avoid malicious messages
exports.serverAuth = {
    incoming: function(message, callback) {
        if (/^\/meta\//.test(message.channel)) {
            return callback(message)
        }

        var password = message.ext && message.ext.password
        if (password !== SERVER_PASSWORD) {
            message.error = faye.Error.extMismatch()
        } else {
            delete message.ext.password;
            callback(message)
        }
    }
}

// Adds the publishing key to the message
exports.clientAuth = {
    outgoing: function(message, callback) {
        message.ext = message.ext || {};
        message.ext.password = SERVER_PASSWORD;
        callback(message);
    }
};

// Authenticates Count Von Count
exports.cvcAuth = function(req, res, next) {
    if(req.query.key === CVC_PASSWORD) {
        next();
    } else {
        res.send(403) // Unauthorized
    }
}
