### P R O J E K T - A R M A ###


Dane <- read.csv("PKB Polski 1994-2015.csv", sep = ";", dec= ".", header = TRUE)
attach(Dane)
data <- as.matrix(Dane)
Y = data;
T = nrow(Y);

# Testy stacjonarnoœci: ADF, KPSS
library(fUnitRoots)
library(tseries)
adfTest(Y, lags = 0, type = "c") 
#Przyjmujemy H0 co jest równoznaczne z niestacjonarnoœci¹ szeregu
PP.test(Y)
#Przyjmujemy H0 co jest równoznaczne z niestacjonarnoœci¹ szeregu
kpss.test(Y)
#Odrzucamy H0 co jest równoznaczne z niestacjonarnoœci¹ szeregu


#Stworzenie szeregu przyrostów
data1 = Y[1:T-1];
data2 = Y[2:T];
Y_przyrosty = data2 - data1;


#Testy stacjonarnoœci dla zmiennej przyrostów zmiennej Y
adfTest(Y_przyrosty, lags = 0, type = "c") 
#Odrzucamy H0 co jest równoznaczne ze stacjonarnoœci¹ szeregu
PP.test(Y_przyrosty)
#Przyjmujemy H0 co jest równoznaczne z niestacjonarnoœci¹ szeregu
kpss.test(Y_przyrosty)
#Przyjmujemy H0 co jest równoznaczne ze stacjonarnoœci¹ szeregu
#Wiêkszoœæ testów sugeruje wystêpowanie stacjonarnoœci przyrostów PKB dla Polski


#indetyfikacja
par(mfrow=c(1,2))
acf(Y_przyrosty, main = "PKB Polski - przyrosty")
pacf(Y_przyrosty, main = "PKB Polski - przyrosty")
#wykres funkcji ACF wskazuje na rz¹d MA równy 0-1
#wykres funkcji PACF wskazuje na rz¹d AR równy 1


#estymacja ARMA(1,1)
model11 <- arima(Y_przyrosty,order=c(1,0,1))
model11

#estymacja ARMA(2,1)
model21 <- arima(Y_przyrosty,order=c(2,0,1))
model21
#Kryterium aic jest mniejsze ni¿ w poprzednim modelu, dlatego model11 jest bardziej wskazany

#estymacja ARMA(1,2)
model12 <- arima(Y_przyrosty,order=c(1,0,2))
model12

#estymacja ARMA(1,0)
model13 <- arima(Y_przyrosty,order=c(1,0,0))
model13

#estymacja ARMA(2,2)
model14 <- arima(Y_przyrosty, order=c(2,0,2))
model14
#Wyniki estymacji modeli ARMA dla ró¿nych rzêdów opóŸniej wskazuj¹, ¿e najlepszym modelem jest ARMA(1,1), poniewa¿ ma zdecydowanie najmniejsz¹ wartoœæ
kryterium informacyjnego Akaikego oraz prawie najwiêksz¹ wartoœæ wartoœæ funkcji wiarygodnoœci (ró¿nica 0,02), które wynosz¹: log likelihood = -61.47,  aic = 130.93


#Diagnostyka modelu11
reszty <- model11$residuals;
ts.plot(reszty);
hist(reszty);
#Rozk³ad reszt przypomina rozk³ad normalny

tsdiag(model11)
Box.test(reszty , lag = 4, type = "Ljung-Box")
Box.test(reszty , lag = 4, type = "Box-Pierce")
#wyniki obu testów wskazuj¹ na niewystêpowanie autokorelacji reszt, poniewa¿ w obu przypadkach nie odrzuca siê H0, która mówi o braku autokorelacji


#testowanie wystêpowania efektu ARCH (test Ljunga-Boxa dla reszty^2)  
ts.plot(reszty^2)
Box.test(reszty^2 , lag = 4, type = "Ljung-Box")
Box.test(reszty^2 , lag = 8, type = "Ljung-Box")
#Wyniki obu testów wskazuj¹ jednoznacznie na niewystêpowanie efektu ARCH


#test Engle efektu ARCH
require(FinTS)
ArchTest(reszty, lag=4)
ArchTest(reszty, lag=6)
ArchTest(reszty, lag=8)
ArchTest(reszty, lag=12)
ArchTest(reszty, lag=16)
#Wyniki testów dla ka¿dego rzêdu opóŸnienia wskazuj¹ na niewystêpowanie efektu ARCH


#test normalnoœci rozk³adu reszt
hist(reszty);
#Rozk³ad reszt przypomina rozk³ad normalny
shapiro.test(reszty);
#Wynik testu Shapiro Wilka wskazuje, ¿e rozk³ad reszt w modelu11 jest normalny


#prognozy dla Y_przyrosty 
prognoza <- predict(model11,n.ahead=8)
prognoza$pred; 
prognoza$s;


ts.plot(Y_przyrosty, col="black", lty=3,  main="Prognoza przyrostów PKB Polski", xlim = c(0,30), ylim = c(0,30), ylab="Wartoœci przyrostów PKB", xlab="Okresy")
lines(prognoza$pred, col="blue", lty=3)
lines(prognoza$pred+2*prognoza$s, col="red", lty=3)
lines(prognoza$pred-2*prognoza$s, col="red", lty=3)


#maksymalizacja funkcji log wiarygonoœci dla ARMA(1,1)

reszty <- as.matrix(reszty);
Y_przyrosty <- as.matrix(Y_przyrosty);
T = nrow(Y_przyrosty);

#funkcja wiarygodnoœci dla ARMA(1,1)

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

#liczy funkcjê log likelihood l(1,1,1,1)
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

#wariancja i b³êdy estyamtorów
Var=diag(-invH)
Var
se_hat_Theta = sqrt(abs(Var))

# waroœci estymatorów i ich b³êdy
# c  phi_1 theta_1 sigma   
hat_Theta$par
se_hat_Theta
# porównaj z wynikiem z funckji arima
model11




### P R O J E K T - G A R C H ###


Dane <- read.csv("dane WIG_20 pkn_orlen.csv", sep = ";", dec= ".", header = TRUE)
attach(Dane)
data <- as.matrix(Dane)
Y = data;
T = nrow(Y);

#Definiowanie zmiennych wchodz¹cych w sk³ad modelu liniowego
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
#Rozk³ad reszt przypomina rozk³ad normalny

tsdiag(model11)
Box.test(reszty , lag = 4, type = "Ljung-Box")
Box.test(reszty , lag = 4, type = "Box-Pierce")
#wyniki obu testów wskazuj¹ na wystêpowanie autokorelacji reszt, poniewa¿ w obu przypadkach odrzuca siê H0, która mówi o braku autokorelacji


#testowanie wystêpowania efektu ARCH (test Ljunga-Boxa dla reszty^2)  
ts.plot(reszty^2)
Box.test(reszty^2 , lag = 4, type = "Ljung-Box")
Box.test(reszty^2 , lag = 4, type = "Ljung-Box")
#Wyniki obu testów wskazuj¹ jednoznacznie na wystêpowanie efektu ARCH, poniewa¿ odrzucamy H0


#test Engle efektu ARCH
require(FinTS)
ArchTest(reszty, lag=4)
ArchTest(reszty, lag=6)
ArchTest(reszty, lag=8)
ArchTest(reszty, lag=12)
ArchTest(reszty, lag=16)
#Wyniki testów dla ka¿dego rzêdu opóŸnienia wskazuj¹ na wystêpowanie efektu ARCH


#test normalnoœci rozk³adu sk³adnika losowego
hist(reszty);
#Rozk³ad reszt przypomina rozk³ad normalny
shapiro.test(reszty);
#Wynik testu Shapiro Wilka wskazuje, ¿e rozk³ad reszt w modelu liniowym nie jest normalny



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
  

     #warunki pocz¹tkowe
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
#liczy funkcjê log likelihood
l_GARCH(c(1,0.5,0.25,1,1,1,1))


hat_Theta1 <- optim(par = c(1, 0.5,0.25,1,1,1,1), fn =l_GARCH, control = list(fnscale = -1))
hat_Theta1


#obliczenie wartoœci teoretycznych zmiennej ln_orlen w równaniu œredniej przy u¿yciu oszacowanych paramterów MNW 
parametry <- as.matrix(hat_Theta1$par);
parametry <- parametry[4:7];
ln_orlen_teor <- rep(0,T-1);

for(n in 1:T){
ln_orlen_teor[n] = parametry[1] + parametry[2] * ln_orlen_opoznione[n] + parametry[3] * ln_WIG_20[n] - parametry[4] * ln_WIG_20_opoznione[n]
}
# wartoœci ln_orlen_teor oraz ln_orlen s¹ bardzo zbli¿one do wartoœci empirycznych



#prognozy dla ln_orlen uzyskanych z modelu szacowanego MNW
ln_orlen_teor <- as.matrix(ln_orlen_teor)
ln_orlen_teor = ln_orlen_teor[4200:T]

model_wygl_wykl1 <- arima(ln_orlen_teor,order=c(2,0,0))

#prognozy dla ln_orlen_teor 
prognoza1 <- predict(model_wygl_wykl1,n.ahead=3)
prognoza1$pred; 
prognoza1$s;


ts.plot(ln_orlen_teor , col="black", lty=3,  main="Prognoza logarytmów notowañ spó³ki PKN Orlen (MNW)", xlim = c(0,90), xlab="Okresy")
lines(prognoza1$pred, col="blue", lty=3)
lines(prognoza1$pred+2*prognoza1$s, col="red", lty=3)
lines(prognoza1$pred-2*prognoza1$s, col="red", lty=3)



#obliczenie wartoœci teoretycznych zmiennej ln_orlen z modelu liniowego oszacowanego MNK 

wspolczynniki <- model_liniowy$coefficients
wspolczynniki <- as.matrix(wspolczynniki)

ln_orlen <- as.matrix(ln_orlen)
T = nrow(ln_orlen);
ln_orlen_teor_MNK <- rep(0,T);

for(n in 1:T){
ln_orlen_teor_MNK[n] = wspolczynniki[1] + wspolczynniki[2] * ln_orlen_opoznione[n] + wspolczynniki[3] * ln_WIG_20[n] + wspolczynniki[4] * ln_WIG_20_opoznione[n]
}
# wartoœci ln_orlen_teor_MNK oraz ln_orlen s¹ bardzo zbli¿one do wartoœci empirycznych


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
ts.plot(ln_orlen_teor_MNK , col="black", lty=3,  main="Prognoza logarytmów notowañ spó³ki PKN Orlen (MNK)", xlim = c(0,90), xlab="Okresy")
lines(prognoza$pred, col="blue", lty=3)
lines(prognoza$pred+2*prognoza$s, col="red", lty=3)
lines(prognoza$pred-2*prognoza$s, col="red", lty=3)

#MNW
ts.plot(ln_orlen_teor , col="black", lty=3,  main="Prognoza logarytmów notowañ spó³ki PKN Orlen (MNW)", xlim = c(0,90), xlab="Okresy")
lines(prognoza1$pred, col="blue", lty=3)
lines(prognoza1$pred+2*prognoza1$s, col="red", lty=3)
lines(prognoza1$pred-2*prognoza1$s, col="red", lty=3)


