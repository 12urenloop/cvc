const request = require("request");
const MANUAL_HOST = "10.0.5.7";
const MANUAL_PORT = 3000;
const BOXXY_HOST = "localhost";
const BOXXY_PORT = 8080;

const config = require("./config.json");

request(`http://${MANUAL_HOST}:${MANUAL_PORT}/teams`, function (error, response, body) {
    const newState = {
        teams: {}
    };
    const state = JSON.parse(body);
    console.log(state);


    state.teams.forEach((team) => {
            const offset = config.offsets[team.id];
            newState.teams[team.id] = {
                id: team.id,
                name: team.name,
                laps: offset + team.status.lapCount,
                updated: team.status.lastBumpAt
            }
        }
    );

    request.put(`https://live.12urenloop.be/state`, {
        json: true, body: newState,
        auth: {
            user: "count-von-count",
            pass: "tetten19",
            sendImmediately: false
        }
    })
        .on("response", (response => {
            console.log(response.statusCode);
            console.log(response.statusMessage);
        }));

});

