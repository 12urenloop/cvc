var SERVER_PASSWORD = "michiel is ne coole kerel",
    Faye = require('faye')

exports.ServerAuth = {
  incoming: function(message, callback) {
    if (/^\/meta\//.test(message.channel)) {
      callback(message)
    }

    var password = message.ext && message.ext.password

    if (password !== SERVER_PASSWORD) {
      message.error = Faye.Error.extMismatch()
    }

    if (password) delete message.ext.password
    callback(message)
  }
}

exports.ClientAuth = {
  outgoing: function(message, callback) {
    message.ext = message.ext || {};
    message.ext.password = SERVER_PASSWORD;
    callback(message);
  }
};
