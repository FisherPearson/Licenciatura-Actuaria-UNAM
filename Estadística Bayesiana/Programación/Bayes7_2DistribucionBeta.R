##################################################
### Ejemplo 6.1 (pag. 172)
### Robert & Casella (2010)
### Introducing Monte Carlo Methods with R
### Springer
##################################################

##################################################
### Packages:
library(rjags)
library(MCMCpack) ### MCMC

##################################################

setwd("~/Desktop/BayesianaEjemplos2019.2/")
getwd()

##################################################

##################################################
### Simular datos
### Algoritmo Metropolis-Hastings

Nsim = 50000
a = 2.7   
b = 6.3   
c = 2.669

X = rep(runif(1),Nsim) # initialize the chain DENSIDAD DE TRANSICION
for(i in 2:Nsim){
	Y = runif(1)   # densidad de transicion
	rho = dbeta(Y,a,b)/dbeta(X[i-1],a,b)
	X[i] = X[i-1] + (Y-X[i-1])*(runif(1)<rho)
}

hist(X,nclass=50,freq=F,xlim=c(0,1))
w = seq(0,1,by=0.001)
dw = dbeta(w,a,b)
lines(w,dw,col="blue",lwd=2)

plot(X,type="l")
plot(as.mcmc(X))

##################################################
### Algoritmo Aceptaci?n y Rechazo

Nsim = 1000
a=2.7; b=6.3
M = 2.67

w = seq(0,1,length.out=1001)
dw = dbeta(w,a,b)
max(dw) ### < M=2.67

y = runif(Nsim) #generation from g
u = runif(Nsim,max=M) #uniform over (0,M)
x = y[u<dbeta(y,a,b)] #accepted subsample

hist(x,nclass=50,freq=F,xlim=c(0,1))
lines(w,dw,col="blue",lwd=2)

##################################################

##################################################
### Accepted rate

sum(X[-1]!=X[-Nsim])/Nsim   ###Metropolis-Hastings
sum(u<dbeta(y,a,b))/Nsim   ###Aceptacion-Rechazo

##################################################
