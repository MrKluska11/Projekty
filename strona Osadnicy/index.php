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
<h3>Osadnicy - gra przeglądarkowa<h3><br/><br/>
<a href="rejestracja.php">Rejestracja - załóż darmowe konto!</a>

<form action="zaloguj.php" method="post">
<br/>
Login: <br/> <input type="text" name="login"/> <br/>
Hasło: <br/> <input type="password" name="haslo"/> <br/><br/>
<input type="submit" value="Zaloguj się"/>

<?php
if(isset($_SESSION['blad'])) echo $_SESSION['blad'];

?>

</form>

</body>

</html>
