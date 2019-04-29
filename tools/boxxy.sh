
send() {
    curl -XPUT https://live.12urenloop.be/state \
         -H 'Content-Type: application/json' \
         -u 'count-von-count:tetten19' \
         -d "$1"
}

case $1 in
    "freeze") send '{"frozen": true}';;
    "melt")   send '{"frozen": false}';;
    *)        send "'$*'";;
esac
