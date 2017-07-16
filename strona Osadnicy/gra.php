<?php

	session_start();
?>

<!DOCTYPE HTML>
<html lang="pl">

<head>
<meta charset="utf-8"/>
<title>Osadnicy - gra przeglądarkowa</title>
</head>

<body>

<?php
echo"<p>Witaj ".$_SESSION['user']."!</p>";
echo"<p><b>Drewno</b>:".$_SESSION['drewno'];
echo"|<b>Kamien</b>:".$_SESSION['kamien'];
echo"|<b>Zboże</b>:".$_SESSION['zboze']."</p>";

echo"<p><b>E-mail</b>: ".$_SESSION['email'];
echo"<br/><b>Dni premium</b>: ".$_SESSION['dnipremium']."</p>";



?>


</body>

</html>
