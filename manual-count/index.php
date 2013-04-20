<?php require 'lib/mysql.php'; ?>
<!DOCTYPE html>
<html>
<head>
    <title>12Urenloop - Manuele telling</title>
    <meta name="viewport" content="width=device-width, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no">

    <link href="style.css" media="screen" rel="stylesheet" type="text/css" />
    <link rel="apple-touch-icon" href="apple-touch-icon.png" />

    <script type="text/javascript" src="jquery-1.9.1.min.js"></script>
    <script type="text/javascript" src="script.js"></script>
    <script type="text/javascript">
        var teams = (<?= json_encode(getTeams()); ?>);
    </script>
</head>
<body ontouchmove="event.preventDefault();" >
    <header>
        <h1>Manuele telling</h1>
        <button id="viewSwitcher">iPad 1/2</button>
        <button id="clearButton">Reset</button>
        <button id="adminButton">Admin</button>
    </header>
	<div id="content">
	    <ul id="buttonHolder">
	    </ul>
	</div>
	<div id="overlay">
		<a id="closeModal"><img src="modal_close.png"/></a>
		<div id="overlayContent">
			<h1>Lastest error</h1>
			<p id="overlayError"></p>
			<h1>Queue</h1>
			<p id="overlayQueue"></p>
		</div>
	</div>
</body>
</html>
