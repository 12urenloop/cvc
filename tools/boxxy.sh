
send() {
    curl -XPUT localhost:8080/state \
         -H 'Content-Type: application/json' \
         -u 'count-von-count:tetten' \
         -d "$1"
}

while test -n "$1"; do
    case "$1" in
        "freeze") send '{"frozen": true}';;
        "melt")   send '{"frozen": false}';;
        "notify") send '{"notification": "'$2'"}'; shift;;
    esac
    shift
done

