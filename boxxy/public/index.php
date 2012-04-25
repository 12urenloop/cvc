<?php include('redirect.php'); ?>
<!DOCTYPE html>
<html>
<head>
    <title>12Urenloop Live (sorta)</title>
    <link rel="stylesheet" type="text/css" href="vstyle.css" />

    <script type="text/javascript" src="lib/jquery-1.7.1.min.js"></script>
    <script type="text/javascript">
        var teams = [{"name":"VTK","baton":"00:12:02:01:06:25","id":"t5","laps":745,"lastLapCompleted":"2012-04-25T18:56:44.287Z","speed":9.783518855347861,"station":{"name":"gyrid-6","mac":"00:24:01:EB:C2:65","position":0},"time":"2012-04-25T18:56:44.287Z","ranking":1},{"name":"HILOK","baton":"20:11:02:15:01:34","id":"t14","laps":744,"lastLapCompleted":"2012-04-25T18:56:27.339Z","speed":7.030322728316828,"station":{"name":"gyrid-4","mac":"00:21:91:F1:EA:11","position":195},"time":"2012-04-25T18:56:54.334Z","ranking":2},{"name":"VLK","baton":"00:12:02:01:07:82","id":"t6","laps":698,"lastLapCompleted":"2012-04-25T18:56:10.303Z","speed":4.0611084358125416,"station":{"name":"gyrid-4","mac":"00:21:91:F1:EA:11","position":195},"time":"2012-04-25T18:56:46.479Z","ranking":3},{"name":"VEK","baton":"00:12:02:01:00:04","id":"t4","laps":617,"lastLapCompleted":"2012-04-25T18:56:36.734Z","speed":5.016200131838279,"station":{"name":"gyrid-6","mac":"00:24:01:EB:C2:65","position":0},"time":"2012-04-25T18:56:36.734Z","ranking":4},{"name":"Wetenschappen & Vlak","baton":"00:12:02:01:08:76","id":"t10","laps":600,"lastLapCompleted":"2012-04-25T18:56:12.637Z","speed":5.130549799582965,"station":{"name":"gyrid-4","mac":"00:21:91:F1:EA:11","position":195},"time":"2012-04-25T18:56:43.406Z","ranking":5},{"name":"Politeia","baton":"20:11:02:15:01:93","id":"t8","laps":579,"lastLapCompleted":"2012-04-25T18:56:02.225Z","speed":4.261014681981627,"station":{"name":"gyrid-4","mac":"00:21:91:F1:EA:11","position":195},"time":"2012-04-25T18:56:42.871Z","ranking":6},{"name":"VRG","baton":"20:11:02:15:01:17","id":"t9","laps":575,"lastLapCompleted":"2012-04-25T18:56:30.299Z","speed":8.379953171298052,"station":{"name":"gyrid-3","mac":"00:22:B0:D0:61:1F","position":143},"time":"2012-04-25T18:56:54.215Z","ranking":7},{"name":"Blandinia","baton":"20:11:02:15:01:67","id":"t7","laps":563,"lastLapCompleted":"2012-04-25T18:55:59.677Z","speed":3.9068839294837505,"station":{"name":"gyrid-4","mac":"00:21:91:F1:EA:11","position":195},"time":"2012-04-25T18:56:39.116Z","ranking":8},{"name":"HomeKonvent","baton":"20:11:02:15:01:77","id":"t1","laps":552,"lastLapCompleted":"2012-04-25T18:56:43.327Z","speed":5.377598497606532,"station":{"name":"gyrid-6","mac":"00:24:01:EB:C2:65","position":0},"time":"2012-04-25T18:56:43.327Z","ranking":9},{"name":"VGK & GFK & VBK","baton":"20:11:02:15:01:83","id":"t12","laps":523,"lastLapCompleted":"2012-04-25T18:55:34.833Z","speed":4.59431849956596,"station":{"name":"gyrid-5","mac":"00:21:91:F1:EA:0E","position":289},"time":"2012-04-25T18:56:41.574Z","ranking":10},{"name":"SeniorenKonvent","baton":"20:11:02:15:01:90","id":"t2","laps":517,"lastLapCompleted":"2012-04-25T18:56:11.376Z","speed":6.059957687510823,"station":{"name":"gyrid-4","mac":"00:21:91:F1:EA:11","position":195},"time":"2012-04-25T18:56:49.503Z","ranking":11},{"name":"VPPK","baton":"00:11:72:14:01:98","id":"t11","laps":508,"lastLapCompleted":"2012-04-25T18:56:30.375Z","speed":3.2702457623666996,"station":{"name":"gyrid-2","mac":"00:21:91:F1:EA:0A","position":77},"time":"2012-04-25T18:56:53.921Z","ranking":12},{"name":"KVHV","baton":"20:11:02:15:01:79","id":"t13","laps":498,"lastLapCompleted":"2012-04-25T18:56:27.731Z","speed":3.994426685384792,"station":{"name":"gyrid-2","mac":"00:21:91:F1:EA:0A","position":77},"time":"2012-04-25T18:56:47.008Z","ranking":13},{"name":"Kofschipclubs","baton":"20:11:02:15:01:42","id":"t3","laps":416,"lastLapCompleted":"2012-04-25T18:56:20.876Z","speed":3.028113159880914,"station":{"name":"gyrid-2","mac":"00:21:91:F1:EA:0A","position":77},"time":"2012-04-25T18:56:46.304Z","ranking":14}];
        window.onload = function() {
            var scoreboard = $('#scoreboard').children().first();
            var laps = 0;
            for(var idx in teams) {
                var team = teams[idx];
                laps += team.laps;
                var elem = $('<tr></tr>').attr('id', team.id);
                elem.append($('<td></td>').addClass('name').html('<span class="teamName">' + team.name + '</div>'));
                elem.append($('<td></td>').addClass('score').html(team.laps));
                scoreboard.append(elem);
            }
            var km = (laps * 380 / 1000).toFixed(2);
            $('#totalLaps').html('' + laps);
            $('#km').html('' + km);
            $('#moonDist').html('' + (km / 3800).toPrecision(5));
            $('#nyla').html('' + (km / 39.32).toFixed(2));
            $('#toothbrush').html('' + (km / 0.0002228).toFixed(0));
        }
    </script>
</head>

<body>

<a id="logo" href="http://12urenloop.be/"><img src="assets/logo-100.png" /></a>
<div class="section-title">Om het spannend te houden, updaten we de scores niet meer. Dit is de laatste tussenstand:</div>
<div class="section-content">
    <!--<span style="width: 100%; display: inline-block; text-align: right;"><a id="autorefresh-toggle" href="#"> Toggle autorefresh</a></span>-->
    <table class="scoreboard" id="scoreboard">
        <tbody>
        </tbody>
    </table>
</div>

<div class="section-title">Fun facts:</div>
<div class="section-content">
    Er zijn in totaal <span id="totalLaps"></span> rondjes gelopen. 
    Dit is <span id="km"></span> km, wat <span id="moonDist"></span>% van
    de afstand tot de maan is, of <span id="nyla"></span>% van de afstand tussen
    New York en Los Angeles. Die afstand overbrug je door gemiddeld
    <span id="toothbrush"></span> tandenborstels op een lijn te leggen.
</div>

</body>

</html>
