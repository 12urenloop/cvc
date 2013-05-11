<?php

try {

  $name = htmlspecialchars($_POST['name']);
  $email = htmlspecialchars($_POST['email']);
  $comment = htmlspecialchars($_POST['comment']);

  if(trim($name) == "" || trim($email) == "" || trim($comment) == "") throw new Exception();

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
