<?php

// MySQL connection
mysql_connect('localhost', 'root', 'root') or die(mysql_error());
mysql_select_db('urenloop') or die(mysql_error());

function getTeams() {
    $q = mysql_query('SELECT id, name, laps FROM teams ORDER BY id ASC');

    $result = array();
    while($row = mysql_fetch_assoc($q)) {
        $result['team-' . $row['id']] = $row;
    }
    return $result;
}

function addLap($teamId, $count, $time) {
    // check if time exists
    $q = mysql_query('SELECT id FROM laps
        WHERE team_id = ' . (int)$teamId . ' AND
              time = FROM_UNIXTIME(' . (int)$time . ')');
    if(mysql_num_rows($q) != 0) return true;
    else {
        $q = mysql_query('INSERT INTO laps (team_id,value,time)
                     VALUES (' . (int)$teamId . ',
                             ' . (int)$count . ',
                             FROM_UNIXTIME(' . (int)$time . '))');
        if(!$q) return false;
        $q = mysql_query('UPDATE teams SET laps = laps + ' . (int)$count . '
                     WHERE id = ' . (int)$teamId);
        return $q;
    }
}
