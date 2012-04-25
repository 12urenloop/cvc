<?php

require_once 'mysql.php';

$urls = array("http://localhost:8000");

function toBoxxy($teamid, $count, $time) {
    $teams = getTeams();
    $team = $teams["t$teamid"];
    $data = array(
        'count' => $count,
        'team' => array(
            'id' => $team['id'],
            'name' => $team['name'],
            'laps' => $team['laps']
        ),
        'time' => date('Y-m-d\TH:i:s.000\Z', $time)
    );
    foreach($urls as $url) {
        $ch = curl_init();
        // set the target url
        curl_setopt($ch, CURLOPT_URL, "$url/$teamid/laps/");
        curl_setopt($ch, CURLOPT_PUT, 1);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode(data));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array('Content-Type: application/json'));
        print_r(json_encode(data));
        $result = curl_exec($ch);
        curl_close($ch);
    }
}
