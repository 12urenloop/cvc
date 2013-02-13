/* Requires the `src/boxxy.js` module to be in scope as `boxxy`. The
 * `make-boxxy-client.sh` script takes care of this.
 *
 * We also require sockets.io to be in scope, this should be taken care of in
 * the HTML. */
exports.connect = function(uri) {
    var boxxyState = boxxy.initialize();
    var socket = io.connect(uri);

    socket.on('state', function(newState) {
        boxxy.putState(boxxyState, newState);
    });

    socket.on('lap', function(lap) {
        boxxy.addLap(boxxyState, lap);
    });

    return boxxyState;
}
