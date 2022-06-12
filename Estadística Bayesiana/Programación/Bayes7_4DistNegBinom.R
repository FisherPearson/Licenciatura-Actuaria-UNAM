##################################################
### Distribucion Binomial Negativa
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
n = 1000
theta = 0.2   ### probabilidad
r = 5   ### number of failures until the experiment is stopped

X = rnbinom(n,size=r,prob=theta)

plot(table(X)/n)
w = seq(0,max(X)*1.2,by=1)
dw = dnbinom(w,size=r,prob=theta)
lines(w,dw,col="blue",lwd=2)

##################################################

##################################################
### Simular datos
### Usando variables latentes

beta = theta/(1-theta)
Ylatent = rgamma(n,shape=r,rate=beta)
Xlatent = c()
for(i in 1:n){
	Xlatent[i] = rpois(1,Ylatent[i])
}

plot(table(Xlatent)/n)
lines(w,dw,col="blue",lwd=2)


##################################################


##################################################
### JAGS

data <- list(
	X = X,
	n = n , 
	r = r,    # number of failures until the experiment is stopped
	a0 = 0.1 ,
	b0 = 0.1 
)

param <- c("theta")

inits <- function(){	list(
  "theta" = runif(1,0,1) 
)	}

fit <- jags.model("DistNegBinom.bug", data, inits, n.chains=3)
update(fit,1000)
sample <- coda.samples(fit, param, n.iter=1000, thin=1)

plot(sample)
summary(sample)
traceplot(sample)


### Usando Variables Latentes

initsLatent <- function(){	list(
  "theta" = runif(1,0,1) ,
  "Y" = rpois(n,mean(X))
)	}

paramLatent <- c(
"theta" 
, "Y[1]", "Y[2]", "Y[3]", "Y[4]", "Y[5]"
, "X[1]", "X[2]", "X[3]", "X[4]", "X[5]"
)

fitLatent <- jags.model("DistNegBinomLatent.bug", data, initsLatent, n.chains=3)
update(fitLatent,1000)
sampleLatent <- coda.samples(fitLatent, paramLatent, n.iter=1000, thin=1)

plot(sampleLatent)
summary(sampleLatent)
traceplot(sampleLatent)

##################################################
##################################################
