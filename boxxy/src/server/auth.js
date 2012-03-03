var SERVER_PASSWORD = 'tetten',
    faye = require('faye')

exports.ServerAuth = {
  incoming: function(message, callback) {
    if (/^\/meta\//.test(message.channel)) {
      callback(message)
    }

    var password = message.ext && message.ext.password
    if (password !== SERVER_PASSWORD) {
      message.error = faye.Error.extMismatch()
    }
    else {
      delete message.ext.password;
      callback(message)
    }
  }
}

exports.ClientAuth = {
  outgoing: function(message, callback) {
    message.ext = message.ext || {};
    message.ext.password = SERVER_PASSWORD;
    callback(message);
  }
};
