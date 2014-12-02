<?php

try {

  $name = trim(htmlspecialchars($_POST['name']));
  $email = trim(htmlspecialchars($_POST['email']));
  $comment = trim(htmlspecialchars($_POST['comment']));

  // Empty detection
  if($name == "" || $email == "" || $comment == "") throw new Exception();

  if (!preg_match("/^([A-Za-z0-9_\-\.])+\@([A-Za-z0-9_\-\.])+\.([A-Za-z]{2,8})$/", $email)){
    throw new Exception();
  }

  // Blacklist certain words
  $blacklist = array("viagra", "erectile", "prescription", "valium",
                     "xanax", "sell", "buy", "rental", "teen", "sex", "car", "offer",
                     "smoke", "cigarette", "medication", "coffee", "pill", "cialis",
                     "monthly", "abuse", "electronic", "woman", "xanax", "medication",
                     "[/url]", "[url");

  $count = 0;
  foreach($blacklist as $needle) {
    // doe alsof mail verzonden is voor spambots en kill script
    if(stristr($name, $needle) != FALSE || stristr($email, $needle) != FALSE || stristr($comment, $needle) != FALSE) 
      $count++;

      if($count > 1) die("Mail verzonden... Mail sent...");
  }


  // Silly spambot detection
  $context  = stream_context_create(array('http' => array('header' => 'Accept: application/xml')));
  $url = 'http://www.stopforumspam.com/api?email=' . $email;
  $xml = file_get_contents($url, false, $context);
  $xml = simplexml_load_string($xml);
  if($xml->appears == "yes") throw new Exception();

  $to = 'info@12urenloop.be';
  $subject = '[12urenloop] Mail via website';
  $headers  = 'MIME-Version: 1.0' . "\r\n";
  $headers .= 'Content-type: text/html; charset=iso-8859-1' . "\r\n";
  $headers .= 'From: website@12urenloop.be' . "\r\n";
  $headers .= 'Reply-To: ' . $email . "\r\n";
  $message = "
    <table>
    <tr><td>Naam: </td><td>" . $name . "</td></tr>
    <tr><td>E-Mail: </td><td>" . $email . "</td></tr>
    <tr><td>Bericht: </td><td>" . $comment . "</td></tr>
    </table>";
  mail($to, $subject, $message, $headers);
  echo "Bericht verzonden! U wordt nu teruggestuurd naar de website.";


} catch (Exception $e) {
  echo "Fout bij verzenden, gelieve opnieuw te proberen. U wordt automatisch teruggestuurd.";
}

header("Refresh: 2;index.html#contact");
