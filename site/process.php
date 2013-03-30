<?php
try {
  //$to = 'info@12urenloop.be';     //put your email address on which you want to receive the information
  $to = 'siloxtom@gmail.com';
  $subject = '[12Urenloop] Mail via website';   //set the subject of email.
  $headers  = 'MIME-Version: 1.0' . "\r\n";
  $headers .= 'Content-type: text/html; charset=iso-8859-1' . "\r\n";
  $headers .= 'From: ' . $_POST['name'] . ' ' . $_POST['email'] . "\r\n";
  $headers .= 'Reply-To: ' . $_POST['email'] . "\r\n";
  $message = "
    <table>
    <tr><td>Naam: </td><td>" . $_POST['name'] . "</td></tr>
    <tr><td>E-Mail: </td><td>" . $_POST['email'] . "</td></tr>
    <tr><td>Bericht: </td><td>" . $_POST['comment'] . "</td></tr>
    </table>";
  mail($to, $subject, $message, $headers);
  echo 1;
} catch (Exception $e) {
  echo 0;
}
?>
