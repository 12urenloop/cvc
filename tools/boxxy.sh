
send() {
    curl -XPUT localhost:8080/state \
         -H 'Content-Type: application/json' \
         -u 'count-von-count:tetten' \
         -d "$1"
}

case $1 in
    "freeze") send '{"frozen": true}';;
    "melt")   send '{"frozen": false}';;
    *)        send "'$*'";;
esac
