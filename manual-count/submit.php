<?php
require 'lib/mysql.php';

if(isset($_POST['team']) && isset($_POST['time'])) {
    echo (int)addLap($_POST['team'], 1, $_POST['time']);
} else {
    echo 0;
}
