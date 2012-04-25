<?php

require_once 'mysql.php';

function toBoxxy($teamid, $count, $time) {
    $urls = array("http://localhost:8000");
    $teams = getTeams();
    $team = $teams["team-$teamid"];
    $laps = (int) $team['laps'];
    $count = (int) $count;
    $data = array(
        'count' => $count,
        'team' => array(
            'id' => "t$teamid",
            'name' => $team['name'],
            'laps' => $laps
        ),
        'time' => date('Y-m-d\TH:i:s.000\Z', $time)
    );
    foreach($urls as $url) {
        $ch = curl_init();
        // set the target url
        echo "$url/$teamid/laps/";
        curl_setopt($ch, CURLOPT_URL, "$url/$teamid/laps/?key=tetten");
        curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "PUT");
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($data));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array('Content-Type: application/json'));
        // print_r(json_encode($data));
        $result = curl_exec($ch);
        curl_close($ch);
    }
}
