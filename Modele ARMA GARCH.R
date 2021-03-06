### P R O J E K T - A R M A ###


Dane <- read.csv("PKB Polski 1994-2015.csv", sep = ";", dec= ".", header = TRUE)
attach(Dane)
data <- as.matrix(Dane)
Y = data;
T = nrow(Y);

# Testy stacjonarno�ci: ADF, KPSS
library(fUnitRoots)
library(tseries)
adfTest(Y, lags = 0, type = "c") 
#Przyjmujemy H0 co jest r�wnoznaczne z niestacjonarno�ci� szeregu
PP.test(Y)
#Przyjmujemy H0 co jest r�wnoznaczne z niestacjonarno�ci� szeregu
kpss.test(Y)
#Odrzucamy H0 co jest r�wnoznaczne z niestacjonarno�ci� szeregu


#Stworzenie szeregu przyrost�w
data1 = Y[1:T-1];
data2 = Y[2:T];
Y_przyrosty = data2 - data1;


#Testy stacjonarno�ci dla zmiennej przyrost�w zmiennej Y
adfTest(Y_przyrosty, lags = 0, type = "c") 
#Odrzucamy H0 co jest r�wnoznaczne ze stacjonarno�ci� szeregu
PP.test(Y_przyrosty)
#Przyjmujemy H0 co jest r�wnoznaczne z niestacjonarno�ci� szeregu
kpss.test(Y_przyrosty)
#Przyjmujemy H0 co jest r�wnoznaczne ze stacjonarno�ci� szeregu
#Wi�kszo�� test�w sugeruje wyst�powanie stacjonarno�ci przyrost�w PKB dla Polski


#indetyfikacja
par(mfrow=c(1,2))
acf(Y_przyrosty, main = "PKB Polski - przyrosty")
pacf(Y_przyrosty, main = "PKB Polski - przyrosty")
#wykres funkcji ACF wskazuje na rz�d MA r�wny 0-1
#wykres funkcji PACF wskazuje na rz�d AR r�wny 1


#estymacja ARMA(1,1)
model11 <- arima(Y_przyrosty,order=c(1,0,1))
model11

#estymacja ARMA(2,1)
model21 <- arima(Y_przyrosty,order=c(2,0,1))
model21
#Kryterium aic jest mniejsze ni� w poprzednim modelu, dlatego model11 jest bardziej wskazany

#estymacja ARMA(1,2)
model12 <- arima(Y_przyrosty,order=c(1,0,2))
model12

#estymacja ARMA(1,0)
model13 <- arima(Y_przyrosty,order=c(1,0,0))
model13

#estymacja ARMA(2,2)
model14 <- arima(Y_przyrosty, order=c(2,0,2))
model14
#Wyniki estymacji modeli ARMA dla r�nych rz�d�w op�niej wskazuj�, �e najlepszym modelem jest ARMA(1,1), poniewa� ma zdecydowanie najmniejsz� warto��
kryterium informacyjnego Akaikego oraz prawie najwi�ksz� warto�� warto�� funkcji wiarygodno�ci (r�nica 0,02), kt�re wynosz�: log likelihood = -61.47,  aic = 130.93


#Diagnostyka modelu11
reszty <- model11$residuals;
ts.plot(reszty);
hist(reszty);
#Rozk�ad reszt przypomina rozk�ad normalny

tsdiag(model11)
Box.test(reszty , lag = 4, type = "Ljung-Box")
Box.test(reszty , lag = 4, type = "Box-Pierce")
#wyniki obu test�w wskazuj� na niewyst�powanie autokorelacji reszt, poniewa� w obu przypadkach nie odrzuca si� H0, kt�ra m�wi o braku autokorelacji


#testowanie wyst�powania efektu ARCH (test Ljunga-Boxa dla reszty^2)  
ts.plot(reszty^2)
Box.test(reszty^2 , lag = 4, type = "Ljung-Box")
Box.test(reszty^2 , lag = 8, type = "Ljung-Box")
#Wyniki obu test�w wskazuj� jednoznacznie na niewyst�powanie efektu ARCH


#test Engle efektu ARCH
require(FinTS)
ArchTest(reszty, lag=4)
ArchTest(reszty, lag=6)
ArchTest(reszty, lag=8)
ArchTest(reszty, lag=12)
ArchTest(reszty, lag=16)
#Wyniki test�w dla ka�dego rz�du op�nienia wskazuj� na niewyst�powanie efektu ARCH


#test normalno�ci rozk�adu reszt
hist(reszty);
#Rozk�ad reszt przypomina rozk�ad normalny
shapiro.test(reszty);
#Wynik testu Shapiro Wilka wskazuje, �e rozk�ad reszt w modelu11 jest normalny


#prognozy dla Y_przyrosty 
prognoza <- predict(model11,n.ahead=8)
prognoza$pred; 
prognoza$s;


ts.plot(Y_przyrosty, col="black", lty=3,  main="Prognoza przyrost�w PKB Polski", xlim = c(0,30), ylim = c(0,30), ylab="Warto�ci przyrost�w PKB", xlab="Okresy")
lines(prognoza$pred, col="blue", lty=3)
lines(prognoza$pred+2*prognoza$s, col="red", lty=3)
lines(prognoza$pred-2*prognoza$s, col="red", lty=3)


#maksymalizacja funkcji log wiarygono�ci dla ARMA(1,1)

reszty <- as.matrix(reszty);
Y_przyrosty <- as.matrix(Y_przyrosty);
T = nrow(Y_przyrosty);

#funkcja wiarygodno�ci dla ARMA(1,1)

y_0 = 0;
eps_0=0;

l <- function(Theta) {
    	c <- Theta[1]  
    	phi_1<-Theta[2] 
	theta_1 <- Theta[3]  
	sigma<-Theta[4]  
      pi = 22/7
	
	eps <- rep(0,T)
	eps[1] = Y_przyrosty[1] - c - phi_1 * y_0 -theta_1*eps_0
	sum=eps[1]^2/(2*sigma^2)
	for (n in 2:T){
	eps[n]=Y_przyrosty[n]-c-phi_1*Y_przyrosty[n-1]-theta_1*eps[n-1]
	sum = sum + eps[n]^2/(2*sigma^2)
	}

	loglike <- -T/2*log(2*pi) - T/2*log(sigma^2) - sum    
    return(loglike)
}

#liczy funkcj� log likelihood l(1,1,1,1)
l(c(14.5598276, -0.1784396,  1.6298357,  2.8323925))


hat_Theta <- optim(par = c(5, 0.5, 0.5,5), fn =l, control = list(fnscale = -1))
hat_Theta
model11

hat_Theta <- optim(par = c(5, 0.5,0.5,5), method = "L-BFGS", fn = l, control = list(fnscale = -1), hessian=TRUE )

# wyniki optymalizacji 
hat_Theta

#parametry
hat_Theta$par

#Hessian
H=hat_Theta$hessian

invH = solve(H) # H^-1 
invH

#wariancja i b��dy estyamtor�w
Var=diag(-invH)
Var
se_hat_Theta = sqrt(abs(Var))

# waro�ci estymator�w i ich b��dy
# c  phi_1 theta_1 sigma   
hat_Theta$par
se_hat_Theta
# por�wnaj z wynikiem z funckji arima
model11




### P R O J E K T - G A R C H ###


Dane <- read.csv("dane WIG_20 pkn_orlen.csv", sep = ";", dec= ".", header = TRUE)
attach(Dane)
data <- as.matrix(Dane)
Y = data;
T = nrow(Y);

#Definiowanie zmiennych wchodz�cych w sk�ad modelu liniowego
ln_orlen = Y[2:T,1];
ln_orlen_opoznione = Y[1:T-1,1];
ln_WIG_20 = Y[2:T,2];
ln_WIG_20_opoznione = Y[1:T-1,2];

#model liniowy
model_liniowy <- lm(ln_orlen ~ ln_orlen_opoznione + ln_WIG_20 + ln_WIG_20_opoznione)
summary(model_liniowy)

#Diagnostyka modelu
reszty <- model_liniowy$residuals;
ts.plot(reszty);
hist(reszty);
#Rozk�ad reszt przypomina rozk�ad normalny

tsdiag(model11)
Box.test(reszty , lag = 4, type = "Ljung-Box")
Box.test(reszty , lag = 4, type = "Box-Pierce")
#wyniki obu test�w wskazuj� na wyst�powanie autokorelacji reszt, poniewa� w obu przypadkach odrzuca si� H0, kt�ra m�wi o braku autokorelacji


#testowanie wyst�powania efektu ARCH (test Ljunga-Boxa dla reszty^2)  
ts.plot(reszty^2)
Box.test(reszty^2 , lag = 4, type = "Ljung-Box")
Box.test(reszty^2 , lag = 4, type = "Ljung-Box")
#Wyniki obu test�w wskazuj� jednoznacznie na wyst�powanie efektu ARCH, poniewa� odrzucamy H0


#test Engle efektu ARCH
require(FinTS)
ArchTest(reszty, lag=4)
ArchTest(reszty, lag=6)
ArchTest(reszty, lag=8)
ArchTest(reszty, lag=12)
ArchTest(reszty, lag=16)
#Wyniki test�w dla ka�dego rz�du op�nienia wskazuj� na wyst�powanie efektu ARCH


#test normalno�ci rozk�adu sk�adnika losowego
hist(reszty);
#Rozk�ad reszt przypomina rozk�ad normalny
shapiro.test(reszty);
#Wynik testu Shapiro Wilka wskazuje, �e rozk�ad reszt w modelu liniowym nie jest normalny



#Estymacja MNW GARCH(1,1)

T = nrow(Y) - 1;

l_GARCH <- function(Theta) {
    	teta_0 <- Theta[1]  
	teta_1 <- Theta[2]  
	phi_1 <-Theta[3]
      a_0 <- Theta[4] 
      a_1 <- Theta[5]
      a_2 <- Theta[6]
      a_3 <- Theta[7]
      pi = 22/7
  

     #warunki pocz�tkowe
     reszty_0 = reszty[1]
     sigma2_0 = 0
     eps <- rep(0,T)
	sigma2 <- rep(0,T)

	sigma2[1] = teta_0 + teta_1 * reszty_0^2 + phi_1 * sigma2_0
	sum = 0.5 * log(sigma2[1]) + reszty[1]^2/(2*sigma2[1])
      ln_orlen_empiryczne = rep(0,T-2)   

	for (n in 2:T){
      ln_orlen[n] =  a_1 * ln_orlen_opoznione[n] + a_2 * ln_WIG_20[n] - a_3 * ln_WIG_20_opoznione[n]
      eps[n] = Y[n,1] - (a_1 * ln_orlen_opoznione[n] + a_2 * ln_WIG_20[n] - a_3 * ln_WIG_20_opoznione[n])

	sigma2[n] = teta_0 + teta_1 * eps[n-1]^2 + phi_1 * sigma2[n-1]
	sum = sum + 0.5 * log(sigma2[n]) + eps[n]^2/(2*sigma2[n])
      
	}

	loglikelihood <- -T/2*log(2*pi) - sum
    return(c(loglikelihood))
}
#liczy funkcj� log likelihood
l_GARCH(c(1,0.5,0.25,1,1,1,1))


hat_Theta1 <- optim(par = c(1, 0.5,0.25,1,1,1,1), fn =l_GARCH, control = list(fnscale = -1))
hat_Theta1


#obliczenie warto�ci teoretycznych zmiennej ln_orlen w r�wnaniu �redniej przy u�yciu oszacowanych paramter�w MNW 
parametry <- as.matrix(hat_Theta1$par);
parametry <- parametry[4:7];
ln_orlen_teor <- rep(0,T-1);

for(n in 1:T){
ln_orlen_teor[n] = parametry[1] + parametry[2] * ln_orlen_opoznione[n] + parametry[3] * ln_WIG_20[n] - parametry[4] * ln_WIG_20_opoznione[n]
}
# warto�ci ln_orlen_teor oraz ln_orlen s� bardzo zbli�one do warto�ci empirycznych



#prognozy dla ln_orlen uzyskanych z modelu szacowanego MNW
ln_orlen_teor <- as.matrix(ln_orlen_teor)
ln_orlen_teor = ln_orlen_teor[4200:T]

model_wygl_wykl1 <- arima(ln_orlen_teor,order=c(2,0,0))

#prognozy dla ln_orlen_teor 
prognoza1 <- predict(model_wygl_wykl1,n.ahead=3)
prognoza1$pred; 
prognoza1$s;


ts.plot(ln_orlen_teor , col="black", lty=3,  main="Prognoza logarytm�w notowa� sp�ki PKN Orlen (MNW)", xlim = c(0,90), xlab="Okresy")
lines(prognoza1$pred, col="blue", lty=3)
lines(prognoza1$pred+2*prognoza1$s, col="red", lty=3)
lines(prognoza1$pred-2*prognoza1$s, col="red", lty=3)



#obliczenie warto�ci teoretycznych zmiennej ln_orlen z modelu liniowego oszacowanego MNK 

wspolczynniki <- model_liniowy$coefficients
wspolczynniki <- as.matrix(wspolczynniki)

ln_orlen <- as.matrix(ln_orlen)
T = nrow(ln_orlen);
ln_orlen_teor_MNK <- rep(0,T);

for(n in 1:T){
ln_orlen_teor_MNK[n] = wspolczynniki[1] + wspolczynniki[2] * ln_orlen_opoznione[n] + wspolczynniki[3] * ln_WIG_20[n] + wspolczynniki[4] * ln_WIG_20_opoznione[n]
}
# warto�ci ln_orlen_teor_MNK oraz ln_orlen s� bardzo zbli�one do warto�ci empirycznych


#prognozy dla ln_orlen uzyskanych z modelu szacowanego MNW
ln_orlen_teor_MNK <- as.matrix(ln_orlen_teor_MNK)
ln_orlen_teor_MNK = ln_orlen_teor_MNK[4200:T]

model_wygl_wykl <- arima(ln_orlen_teor_MNK, order=c(2,0,0))

#prognozy dla ln_orlen_teor_MNK
prognoza <- predict(model_wygl_wykl,n.ahead=3)
prognoza$pred; 
prognoza$s;

par(mfrow=c(1,2))
#MNK
ts.plot(ln_orlen_teor_MNK , col="black", lty=3,  main="Prognoza logarytm�w notowa� sp�ki PKN Orlen (MNK)", xlim = c(0,90), xlab="Okresy")
lines(prognoza$pred, col="blue", lty=3)
lines(prognoza$pred+2*prognoza$s, col="red", lty=3)
lines(prognoza$pred-2*prognoza$s, col="red", lty=3)

#MNW
ts.plot(ln_orlen_teor , col="black", lty=3,  main="Prognoza logarytm�w notowa� sp�ki PKN Orlen (MNW)", xlim = c(0,90), xlab="Okresy")
lines(prognoza1$pred, col="blue", lty=3)
lines(prognoza1$pred+2*prognoza1$s, col="red", lty=3)
lines(prognoza1$pred-2*prognoza1$s, col="red", lty=3)


