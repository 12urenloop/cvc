var offset = 0

exports.ISODateString = function(date) {
    var d = date || new Date()
    function pad(n){return n<10 ? '0'+n : n}
    function padm(n){return (n < 100 ? '0' : '') + pad(n)}
    return d.getUTCFullYear()+'-'
        + pad(d.getUTCMonth()+1)+'-'
        + pad(d.getUTCDate())+'T'
        + pad(d.getUTCHours())+':'
        + pad(d.getUTCMinutes())+':'
        + pad(d.getUTCSeconds())+'.'
        + padm(d.getUTCMilliseconds()) + 'Z'
}

exports.getTime = function(date) {
    date = date || new Date()
    return new Date(date - offset)
}

exports.synchronize = function(date) {
    offset = new Date() - date
}