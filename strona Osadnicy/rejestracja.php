<?php
session_start();

    if(isset($_POST['email']))
	{
	  $wszystko_OK = true;
	  
	  $nick = $_POST['nick'];
	  
	    //sprawdzenie długości nicka
	    if((strlen($nick) < 3) || (strlen($nick) > 20))
		{
		  $wszystko_OK = false;
		  $_SESSION['e_nick'] = "Nick powinien zawierać od 3 do 20 znaków!";
		
		}
	  
	    //sprawdzenie czy znaki w nicku są alfanumeryczne
	    if(ctype_alnum($nick) == false)
		{
		 $wszystko_OK = false;
		 $_SESSION['e_nick'] = "Nick może składać się tylko z liter i cyfr (bez polskich znaków)";
		 
		}
		
		//sprawdź poprawność adresu e-mail
		$email = $_POST['email'];
		$emailB = filter_var($email, FILTER_SANITIZE_EMAIL);
		if((filter_var($emailB, FILTER_VALIDATE_EMAIL)==false) || ($emailB!=$email))
		{
		  $wszystko_OK = false;
          $_SESSION['e_email'] = "Podaj poprawny adres e-mail";		 
		  
		}
          
          //sprawdź poprawność hasła
          $haslo1 = $_POST['haslo1'];
          $haslo2 = $_POST['haslo2'];
        if((strlen($haslo1) < 8) || (strlen($haslo1) > 20))
        {
          $wszystko_OK = false;
		  $_SESSION['e_haslo'] = "Hasło musi zawierać od 8 do 20 znaków!";
		
        }		
	  
	    if($haslo1!=$haslo2)
		{
		  $wszystko_OK = false;
		  $_SESSION['e_haslo'] = "Podane hasła nie są identyczne";
		
		}
	  
	   //połączenie z bazą danych
	    require_once "connect.php";
		try
		{
		  $polaczenie = new mysqli($host, $db_user, $db_password, $db_name);
		    if($polaczenie->connect_errno!=0)
			{
			  throw new Exception(mysqli_connect_errno());
			}
		    
			//sukces połączenia
			else
			{
			    //czy mail jest zarezerwowany
			   $rezultat = $polaczenie->query("SELECT id FROM uzytkownicy WHERE email='$email'");
			   $ile_takich_maili = $rezultat->num_rows;
			    if($ile_takich_maili->num_rows>0)
			    {
				  $wszystko_OK = false;
			      $_SESSION['e_email'] = "Podany e-mail jest już zajęty";
				
			    }
			    
				//czy nick jest zarezerwowany
			   $rezultat = $polaczenie->query("SELECT id FROM uzytkownicy WHERE user='$nick'");
			   $ile_takich_nickow = $rezultat->num_rows;
			    if($ile_takich_nickow->num_rows>0)
			    {
				  $wszystko_OK = false;
			      $_SESSION['e_nick'] = "Podany nick jest już zajęty";
				
			    }
				
				if($wszystko_OK==true)
				{
				    if($polaczenie->query("INSERT INTO uzytkownicy VALUES(NULL, '$nick', '$haslo1', '$email', 100, 100, 100, 14)"))
					{
					  
					 header('Location: witamy.php');
					}
					
					else
                    {				  
				     throw new Exception($polaczenie->error);
				    }
				  
				  
				
				}
				
				
				
				
			
			}
		
		  $polaczenie->close();
		}
		
		catch(Exception $e)
		{
		  echo '<span style="color: red;">Błąd serwera! Przepraszamy za niedogodności i prosimy o rejestrację w innym terminie!</span>';
		  echo '<br/>Informacja developerska:'.$e;
		}  
	
	
	}



?>







<!DOCTYPE HTML>
<html lang="pl">

<head>
<meta charset="utf-8"/>
<title>Osadnicy - załóż darmowe konto</title>

<style>
.error
{
color: red;
margin-top: 10px;
margin-bottom: 10px;
}

</style>

</head>

<body>

<form method="post">

Nickname:
<br/><input type="text" name="nick"><br/>

    <?php
        if(isset($_SESSION['e_nick']))
		{
		
         echo '<div class="error">'.$_SESSION['e_nick'].'</div>';
		 unset($_SESSION['e_nick']);
		 
        }

    ?> 

E-mail:
<br/><input type="text" name="email"><br/>

    <?php
	    if(isset($_SESSION['e_email']))
		{
		  echo'<div class="error">'.$_SESSION['e_email'].'</div>';
		  unset($_SESSION['e_email']);
		
		}
	
	?>

Twoje hasło:
<br/><input type="password" name="haslo1"><br/>

    <?php
	    if(isset($_SESSION['e_haslo']))
		{
		  echo'<div class="error">'.$_SESSION['e_haslo'].'</div>';
		  unset($_SESSION['e_haslo']);
		
		}
	
	?>

Powtórz hasło:
<br/><input type="password" name="haslo2"><br/>

<br/><input type = "submit" value="Zarejestruj się">


</form>

</body>

</html>