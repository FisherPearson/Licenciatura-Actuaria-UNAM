library(MCMCpack)
library(plyr)

################################# UNIFORME
### Simular datos
### Algoritmo Metropolis-Hastings

Nsim = 50000
mu = 1 #a = 2.7   
sigma = sqrt(.16) #b = 6.3   
 #c = 2.669

X = rep(runif(1),Nsim) # initialize the chain DENSIDAD DE TRANSICION
for(i in 2:Nsim){
  Y = runif(1)   # densidad de transicion
  rho = min(dnorm(Y,mu,sigma)*exp(7*Y)*(1/(1+exp(Y)))^10/(dnorm(X[i-1],mu,sigma)*exp(7*X[i-1])*(1/(1+exp(X[i-1])))^10),1)    #dbeta(Y,a,b)/dbeta(X[i-1],a,b)
  X[i] = X[i-1] + (Y-X[i-1])*(runif(1)<rho)
}

hist(X,nclass=50,freq=F,xlim=c(0,1))
w = seq(0,1,by=0.001)
dw = dnorm(w,mu,sigma)
lines(w,dw,col="blue",lwd=2)

plot(X,type="l")
plot(as.mcmc(X))

################################# NORMAL
### Simular datos
### Algoritmo Metropolis-Hastings

Nsim = 50000
mu = 1 #a = 2.7   
sigma = sqrt(.16) #b = 6.3   
#c = 2.669

X = rep(rnorm(1, mu,sigma),Nsim) # initialize the chain DENSIDAD DE TRANSICION
for(i in 2:Nsim){
  Y = rnorm(1,mu,sigma)   # densidad de transicion
  rho = min(dnorm(X[1, j-1],mu,sigma)*exp(7*y)*(1/(1+exp(y)))^10/(dnorm(y,mu,sigma)*exp(7*X[j-1])*(1/(1+exp(X[j-1])))^10),1)
  X[i] = X[i-1] + (Y-X[i-1])*(runif(1)<rho)
}

hist(X,nclass=50,freq=F,xlim=c(0,1))


plot(X,type="l")
plot(as.mcmc(X))


############################# YADIRA
library(MCMCpack)
library(plyr)

N = 500000

pbar = create_progress_bar('text')
pbar$init(N)

mu = 1    
sigma = sqrt(.16)

X = array(0, dim=c(1,N))

X[1,] = rep(rnorm(n = 1, mean = mu, sd = 0.001 ), N)

for (j in 2:N) {
  y = rnorm(n = 1, mean = mu, sd = 0.01) #rnorm(n = 1, mean = mu, sd = sigma)  
  alpha = min(dnorm(X[1, j-1],mu,sigma)*exp(7*y)*(1/(1+exp(y)))^10/(dnorm(y,mu,sigma)*exp(7*X[j-1])*(1/(1+exp(X[j-1])))^10),1)
  X[1,j] = X[1, j-1] + (y[1] - X[1,j-1]) * (runif(1)<alpha)
  pbar$step()
}

plot(X[1,], type = "l")

hist(X[1,])

plot(as.mcmc(X[1,]))

as.vector(X)



########################### YADIRA CORREGIDO

N = 50000

mu = 1    
sigma = sqrt(.16)

X = array(0, dim=c(1,N))

X[1,] = rep(rnorm(n = 1, mean = mu, sd = 0.001 ), N)

for (j in 2:N) {
  y = rnorm(n = 1, mean = mu, sd = 0.01) 
  alpha = min(dnorm(X[1, j-1],mu,sigma)*exp(7*y)*(1/(1+exp(y)))^10/(dnorm(y,mu,sigma)*exp(7*X[j-1])*(1/(1+exp(X[j-1])))^10),1)
  X[1,j] = X[1, j-1] + (y[1] - X[1,j-1]) * (runif(1)<alpha)
}


par(mfrow = c(1,2))
plot(X[1,], type = "l", main = "Traza", xlab = "Iteraciones", ylab = "", col = "green")
hist(X[1,], main = "Histograma", col = "green", xlab = "Distribución" )


mean_hastings = mean(as.vector(X))
sd_hastings = sd(as.vector(X))

var(as.vector(X))

########################## JAGS
library(rjags)

data <- list(
  n = 10, 
  theta0 = 1,
  tau2inv = 1/.16
)

param <- c("theta")

fit <- jags.model("E1_T4.bug", data,n.chains=3) 

update(fit,1000)

sample <- coda.samples(fit, param, n.iter=5000, thin=1)

plot(sample)
summary(sample)

traceplot(sample)

