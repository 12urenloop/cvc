<?php
// MySQL connection
mysql_connect("localhost", "urenloop", "") or die(mysql_error());
mysql_select_db("urenloop") or die(mysql_error());

if(isset($_GET['f']) && $_GET['f'] == 'init') {
	$q = mysql_query("SELECT * FROM ul_laps");
	$result_arr = array();
	$result_arr['num_teams'] = mysql_num_rows($q);
	$result_arr['teams'] = array();
	
	while($row = mysql_fetch_assoc($q)) {
		$temp = array();
		$temp['id'] = $row['id'];
		$temp['naam'] = $row['naam'];
		$temp['laps'] = $row['laps'];
		array_push($result_arr['teams'], $temp);
	}
	
	echo json_encode($result_arr);
}

if(isset($_POST['f']) && isset($_POST['cid']) && $_POST['f'] == 'count') {
	mysql_query("UPDATE ul_laps SET laps = laps + 1 WHERE id = '".$_POST['cid']."'") or die(mysql_error());
}
?>