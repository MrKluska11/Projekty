#Optymalizacja polityki pieniê¿nej


#x - luka produkcyjna (logarytmy)
#pi - odchylenie inflacji od celu inflacyjnego mierzone wskaŸnikiem CPI
#q - odchylenie realnego kursu walutowego od d³ugookresowego trendu 
#oil - odchylenie ceny ropy naftowej od d³ugookresowego trendu (logarytmy, egzogeniczna)
#i - odchylenie stopy WIBOR 1 mies od jej d³ugookresowego trendu
#e_x, e_pi, e_q - szoki


Dane <- read.csv("Dane_polityka_pieniezna.csv", sep = ";", dec= ".", header = TRUE)
attach(Dane)
data <- as.matrix(Dane)

T = nrow(data)-1;
x_zero = data[3:1,1:3];
x_zero <- as.matrix(x_zero)
N = nrow(x_zero);

x = data[2:T,1]
pi = data[2:T,2]
qq = data[2:T,3]
oil = data[2:T,4]
i = data[2:T,5]
e_x = data[2:T,6]
e_pi = data[2:T,7]
e_q = data[2:T,8]
gamma = 0.99
C = diag(c(1,1,1),3,3);


# parametry c A B
parametry <- matrix(c(-0.000158, 0.847238, 0.052663, 0.004416, -0.191783, 0.000108, -0.0000442,
                   -0.000665, 0.435997, 0.738927, -0.029449, -0.208016, 0.000139, -0.000122,
                    0.005583, 0.82567, -0.554971, 0.612126, 1.657496, 0.000572, -0.000546  ), nrow = 7, ncol = 3);    

x_teor <- rep(0, T)
pi_teor <- rep(0, T)
q_teor <- rep(0, T)


for(n in 3:T){
  x_teor[n] = parametry[1,1] + parametry[2,1] * x[n-1] + parametry[3,1] * pi[n-1] + parametry[4,1] * qq[n-1] + parametry[5,1] * i[n-1] + parametry[6,1] * oil[n-1] + parametry[7,1] * oil[n-2] + e_x[n]
  
  pi_teor[n] = parametry[1,2] + parametry[2,2] * x[n-1] + parametry[3,2] * pi[n-1] + parametry[4,2] * qq[n-1] + parametry[5,2] * i[n-1] + parametry[6,2] * oil[n-1] + parametry[7,2] * oil[n-2] + e_pi[n]
  
  q_teor[n] = parametry[1,3] + parametry[2,3] * x[n-1] + parametry[3,3] * pi[n-1] + parametry[4,3] * qq[n-1] + parametry[5,3] * i[n-1] + parametry[6,3] * oil[n-1] + parametry[7,3] * oil[n-2] + e_q[n]
}

par(mfrow=c(3,2))
ts.plot(x, main = "luka produkcyjna - empiryczne")
ts.plot(x_teor, main = "luka produkcyjna - teoretyczne")
ts.plot(pi, main = "odchylenie inflacji od celu inflacyjnego - empiryczne")
ts.plot(pi_teor, main = "odchylenie inflacji od celu inflacyjnego - teoretyczne")
ts.plot(qq, main = "odchylenie realnego kursu walutowego od d³ugookresowego trendu - empiryczne")
ts.plot(q_teor, main = "odchylenie realnego kursu walutowego od d³ugookresowego trendu - teoretyczne")



optymalne_parametry <- function(parametry_funkcji){
  
  ##funkcja straty
 
  
  lambda <- parametry_funkcji[1]
  p <- parametry_funkcji[2]
  R <- parametry_funkcji[3]
  
  L <- 0.5 * abs(lambda) * sum(x^2) + sum(pi^2) + abs(p) * sum(qq^2) + 0.5 * abs(R) * sum(i[2:T-1]^2)
  
  
  return(c(L))
  
}
parametry_funkcji <- optim(par = c(1, 1, 1), fn = optymalne_parametry) #minimalizacja funkcji optymalna_polityka (czyli bez kontrol = list(fnscale=-1))
parametry_funkcji$par
parametry_funkcji <- as.matrix(parametry_funkcji$par)


optymalna_polityka <- function(parametry_funkcji){

#model transmisji monetarnej


##Inicjalizacja
K = array(0, dim = c(N, N, T+1));
B = parametry[5,];
A = parametry[2:4,];
lambda <- parametry_funkcji[1]
p <- parametry_funkcji[2]
R <- parametry_funkcji[3]
Q = diag(c(lambda, 1, p), 3, 3);
I = diag(1, 3, 3);
E = data[,6:8]  #szoki
E <- as.matrix(E)
c = ncol(B)
u_star = array(0, dim = c(c,T));
x_star = array(0, dim = c(N,N, T));
V = array(0, dim = c(1,T));

u_star[1] = 0


D = array(1, dim=c(N,T+1));
pp <- rep(0,T+1);
calR = array(0, dim = c(c, c, T))
q=1
pp[T+1] = q;
v = array(0, dim = c(T+1));

##warunki brzegowe
K[,,T+1] = Q;  

             

##Rekursja Riccatiego
for(n in c(T: -1: 1)){
K[,,n] = t(A) %*% K[,,n+1] %*% (I - B %*% solve(R + t(B) %*% K[,,n+1] %*% B) %*% t(B) %*% K[,,n+1]) %*% A + Q  #uwzglêdni³em Q w nastêpnym równaniu, poniewa¿ gdy zosta³o uwzglêdnione w tym równaniu to niezgadzaj¹ siê wtedy tablice
calR[n] = solve(R + gamma * t(B) %*% K[,,n+1] %*% B);
pp[n] = -(gamma * t(A) %*% K[,,n+1] %*% B) %*% calR[n] %*% (gamma * t(B) %*% (K[,,n+1] %*% D[,n+1] + pp[n+1])) + gamma * t(A) %*% (K[,,n+1] %*% D[,n+1] + pp[n+1]) + q;
v[n] = gamma * v[n+1] + 0.5 * gamma * t(K[,,n+1] %*% D[,n+1] + pp[n+1]) %*% D[,n+1] + 
0.5 * gamma * sum(diag(K[,,n+1] %*% C %*% t(C))) - 0.5 * t(calR[n] %*% (gamma *t(B) %*% (K[,,n+1] %*% D[,n+1] + pp[n+1]))) %*%
(gamma * t(B) %*% (K[,,n+1] %*% D[,n+1] + pp[n+1]));

}


##Rozwi¹zanie
for(n in 2:T){
x_star[,,1] = x_zero
x_star[,,n] = (A - B %*% solve(R + t(B) %*% K[,,n] %*% B) %*% t(B) %*% K[,,n] %*% A) %*% x_star[,,n-1] + E[n,]  

u_star[n] = - solve(R + t(B) %*% K[,,n] %*% B) %*% t(B) %*% K[,,n] %*% A %*% x_star[,,n]
V[n] = 0.5 * t(K[,,n] %*% x_star[,,n]) %*% x_star[,,n] %*% x_star[,,n] + v[n];

}


return(c(u_star = u_star, x_star = x_star, V=V))

#V=?
}


optymalna_polityka(parametry_funkcji)










# # # S C E N A R I U S Z II # # #          


parametry <- matrix(c(-0.000158, 0.847238, 0.052663, 0.004416, -0.35, 0.000108, -0.0000442,
                   -0.000665, 0.435997, 0.738927, -0.029449, -0.35, 0.000139, -0.000122,
                    0.005583, 0.82567, -0.554971, 0.612126, 2.5, 0.000572, -0.000546  ), nrow = 7, ncol = 3);   


Dane <- read.csv("Dane_polityka_pieniezna.csv", sep = ";", dec= ".", header = TRUE)
attach(Dane)
data <- as.matrix(Dane)

T = nrow(data)-1;
x_zero = data[3:1,1:3];
x_zero <- as.matrix(x_zero)
N = nrow(x_zero);

x = data[2:T,1]
pi = data[2:T,2]
qq = data[2:T,3]
oil = data[2:T,4]
i = data[2:T,5]
e_x = data[2:T,6]
e_pi = data[2:T,7]
e_q = data[2:T,8]
gamma = 0.99
C = diag(c(1,1,1),3,3);


x_teor <- rep(0, T)
pi_teor <- rep(0, T)
q_teor <- rep(0, T)


for(n in 3:T){
  x_teor[n] = parametry[1,1] + parametry[2,1] * x[n-1] + parametry[3,1] * pi[n-1] + parametry[4,1] * qq[n-1] + parametry[5,1] * i[n-1] + parametry[6,1] * oil[n-1] + parametry[7,1] * oil[n-2] + e_x[n]
  
  pi_teor[n] = parametry[1,2] + parametry[2,2] * x[n-1] + parametry[3,2] * pi[n-1] + parametry[4,2] * qq[n-1] + parametry[5,2] * i[n-1] + parametry[6,2] * oil[n-1] + parametry[7,2] * oil[n-2] + e_pi[n]
  
  q_teor[n] = parametry[1,3] + parametry[2,3] * x[n-1] + parametry[3,3] * pi[n-1] + parametry[4,3] * qq[n-1] + parametry[5,3] * i[n-1] + parametry[6,3] * oil[n-1] + parametry[7,3] * oil[n-2] + e_q[n]
}

par(mfrow=c(3,2))
ts.plot(x, main = "luka produkcyjna - empiryczne")
ts.plot(x_teor, main = "luka produkcyjna - teoretyczne")
ts.plot(pi, main = "odchylenie inflacji od celu inflacyjnego - empiryczne")
ts.plot(pi_teor, main = "odchylenie inflacji od celu inflacyjnego - teoretyczne")
ts.plot(qq, main = "odchylenie realnego kursu walutowego od d³ugookresowego trendu - empiryczne")
ts.plot(q_teor, main = "odchylenie realnego kursu walutowego od d³ugookresowego trendu - teoretyczne")



optymalne_parametry <- function(parametry_funkcji){
  
  ##funkcja straty
 
  
  lambda <- parametry_funkcji[1]
  p <- parametry_funkcji[2]
  R <- parametry_funkcji[3]
  
  L <- 0.5 * abs(lambda) * sum(x^2) + sum(pi^2) + abs(p) * sum(qq^2) + 0.5 * abs(R) * sum(i[2:T-1]^2)
  
  
  return(c(L))
  
}
parametry_funkcji <- optim(par = c(1, 1, 1), fn = optymalne_parametry) #minimalizacja funkcji optymalna_polityka (czyli bez kontrol = list(fnscale=-1))
parametry_funkcji$par
parametry_funkcji <- as.matrix(parametry_funkcji$par)


optymalna_polityka <- function(parametry_funkcji){

#model transmisji monetarnej


##Inicjalizacja
K = array(0, dim = c(N, N, T+1));
B = parametry[5,];
A = parametry[2:4,];
lambda <- parametry_funkcji[1]
p <- parametry_funkcji[2]
R <- parametry_funkcji[3]
Q = diag(c(lambda, 1, p), 3, 3);
I = diag(1, 3, 3);
E = data[,6:8]  #szoki
E <- as.matrix(E)
c = ncol(B)
u_star = array(0, dim = c(c,T));
x_star = array(0, dim = c(N,N, T));
V = array(0, dim = c(1,T));

u_star[1] = 0


D = array(1, dim=c(N,T+1));
pp <- rep(0,T+1);
calR = array(0, dim = c(c, c, T))
q=1
pp[T+1] = q;
v = array(0, dim = c(T+1));

##warunki brzegowe
K[,,T+1] = Q;  

             

##Rekursja Riccatiego
for(n in c(T: -1: 1)){
K[,,n] = t(A) %*% K[,,n+1] %*% (I - B %*% solve(R + t(B) %*% K[,,n+1] %*% B) %*% t(B) %*% K[,,n+1]) %*% A + Q  #uwzglêdni³em Q w nastêpnym równaniu, poniewa¿ gdy zosta³o uwzglêdnione w tym równaniu to niezgadzaj¹ siê wtedy tablice
calR[n] = solve(R + gamma * t(B) %*% K[,,n+1] %*% B);
pp[n] = -(gamma * t(A) %*% K[,,n+1] %*% B) %*% calR[n] %*% (gamma * t(B) %*% (K[,,n+1] %*% D[,n+1] + pp[n+1])) + gamma * t(A) %*% (K[,,n+1] %*% D[,n+1] + pp[n+1]) + q;
v[n] = gamma * v[n+1] + 0.5 * gamma * t(K[,,n+1] %*% D[,n+1] + pp[n+1]) %*% D[,n+1] + 
0.5 * gamma * sum(diag(K[,,n+1] %*% C %*% t(C))) - 0.5 * t(calR[n] %*% (gamma *t(B) %*% (K[,,n+1] %*% D[,n+1] + pp[n+1]))) %*%
(gamma * t(B) %*% (K[,,n+1] %*% D[,n+1] + pp[n+1]));

}


##Rozwi¹zanie
for(n in 2:T){
x_star[,,1] = x_zero
x_star[,,n] = (A - B %*% solve(R + t(B) %*% K[,,n] %*% B) %*% t(B) %*% K[,,n] %*% A) %*% x_star[,,n-1] + E[n,]  

u_star[n] = - solve(R + t(B) %*% K[,,n] %*% B) %*% t(B) %*% K[,,n] %*% A %*% x_star[,,n]
V[n] = 0.5 * t(K[,,n] %*% x_star[,,n]) %*% x_star[,,n] %*% x_star[,,n] + v[n];

}

par(mfrow=c(1,3))
ts.plot(x_star[1,1,])
lines(x)

ts.plot(x_star[1,2,])
lines(pi)

ts.plot(x_star[1,3,])
lines(qq)

return(c(u_star = u_star, x_star = x_star, V=V))

#V=?
}


optymalna_polityka(parametry_funkcji)





