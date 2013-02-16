![Boxxy](http://i.imgur.com/5Wet3.png)

Boxxy
=====

Boxxy is the application which powers <http://live.12urenloop.be>. It
functionality can be divided into two big parts:

- Receive info and updates from `count-von-count`
- Serve this information, together with visualisations to the users

Installing & running
====================

Install all dependencies using

    npm install

Run the server with

    node src/server.js

Freezing and setting notifications
==================================

`boxxy` allows displaying a notification to the users. This can be done using:

    curl -XPUT localhost:8080/state \
        -H 'Content-Type: application/json' \
        -u 'count-von-count:tetten' \
        -d '{"notification": "This is a message"}'

At some point close to the end of the competition, we may want to freeze the
displayed scores. This can be done using:

    curl -XPUT localhost:8080/state \
        -H 'Content-Type: application/json' \
        -u 'count-von-count:tetten' \
        -d '{"frozen": true}'

Unfreezing can be done by setting the same parameter to `false`.
