<!DOCTYPE html>
<html>
    <head>
        <title>Maak mij</title>
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <meta charset="utf-8">

        <!-- Bootstrap -->
        <link href="css/bootstrap.min.css" rel="stylesheet" media="screen">
    </head>
    <body>
        <h1>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Gegevens</h1>
        <form class="form-horizontal" action="generate.php" method="post">
            <div class="control-group">
                <label class="control-label" for="inputEmail">Voornaam Achternaam</label>
                <div class="controls">
                    <input type="text" name="naam" placeholder="Voornaam Achternaam" required>
                </div>
            </div>
            <div class="control-group">
                <label class="control-label" for="inputPassword">Functie</label>
                <div class="controls">
                    <input type="text" name="functie" placeholder="Functie" required>
                </div>
            </div>
            <div class="control-group">
                <label class="control-label" for="inputPassword">GSM-nummer</label>
                <div class="controls">
                    <input type="text" name="gsm" placeholder="+32 (0)444 123 456" required>
                </div>
            </div>
            <div class="control-group">
                <div class="controls">
                    <button type="submit" class="btn">Maak signature</button>
                </div>
            </div>
        </form>


        <script src="http://code.jquery.com/jquery.js"></script>
        <script src="js/bootstrap.min.js"></script>
    </body>
</html>
