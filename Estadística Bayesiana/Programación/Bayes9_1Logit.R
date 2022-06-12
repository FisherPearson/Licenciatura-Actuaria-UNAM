##################################################
### LOGIT
##################################################

### Packages:
library(rjags)
library(mnormt)
library(MCMCpack) ### MCMC
library(LearnBayes) ### Bayes

setwd("~/SEMESTRE 2022-2/Estadística Bayesiana/Programación/")
getwd()

##################################################
##################################################
### Simular datos
n = N = 100   # numero de sujetos
K = 3   # numero de parametros de regresion
betas = as.vector(c(3,3,-3))

set.seed(12345)
x1 = runif(n)
x2 = runif(n)
X = cbind(x1,x2,1)
eta = betas[3] + betas[1]*x1 + betas[2]*x2

p = plogis(eta)   ### logistic
p1 = exp(eta)/(1+exp(eta)) 
#u = runif(n)
#y = ifelse(p>u,1,0)
y = rep(NA,n)
for(i in 1:n){
  y[i] = rbinom(1,size=1,prob=p[i])
}
Y = as.vector(y)
plot(eta,y)
points(eta,p,col="blue")
pairs(cbind(y,x1,x2))

##################################################
### Ajuste Frecuentista

mod <- glm(y ~ x1+x2, family=binomial(link="logit"))
plot(mod)
summary(mod)

##################################################
### Usando paquete de R: MCMCpack

modMCMC <- MCMClogit(y~x1+x2, burnin=1000, mcmc=1000, thin=1) 
summary(modMCMC)
plot(modMCMC)


##################################################
### JAGS

data <- list(
	y = y ,
	x1 = x1 ,
	x2 = x2 ,
	n = n  
)

param <- c("Beta")


### Logit

inits <- function(){	list(
  "Beta" = rnorm(3,0,0.1) 
)	}

fit <- jags.model("Bayes9_1Logit.bug", data, inits,  n.chains=3)

update(fit,2000)

sample <- coda.samples(fit, param, n.iter=2000, thin=1)

plot(sample)
summary(sample)

par(mfrow=c(2,2))
traceplot(sample)


dic.samples(fit, n.iter=2000,thin=1, type="pD")

dic.samples(fit, n.iter=2000,thin=1, type="popt")




### Logit Augmented Data

initsA <- function(){	list(
  "Beta" = rnorm(3,0,0.1) , 
  "y.star" = rep(0,n) 
)	}

fitA <- jags.model("LogitAug.bug", data, initsA,  n.chains=3)

update(fitA,2000)

sampleA <- coda.samples(fitA, param, n.iter=2000, thin=1)

plot(sampleA)
summary(sampleA)

par(mfrow=c(2,2))
traceplot(sampleA)



##################################################
##################################################

##################################################
### MCMC 
### Metropolis-Hasting

MHstepLogBeta <- function(betasold,betasnew,Y,X,bv){
	f1old = pmnorm(t(betasold), mean=rep(0,K),varcov=bv)
	f1new = pmnorm(t(betasnew), mean=rep(0,K),varcov=bv)
	f2old = plogis(X%*%betasold)
	f2new = plogis(X%*%betasnew)
	f3old = f2old*Y + (1-f2old)*(1-Y)
	f3new = f2new*Y + (1-f2new)*(1-Y)
	f1 = f1old[1]/f1new[1]
	f0 = ifelse(is.finite(f1),f1,1)
	f3 = f3new/f3old
	f4 = prod(ifelse(is.finite(f3),f3,1))
	ff = f4
	return(ff)
}



SIM = 1000

BETA31 = matrix(0,SIM,K)

### Iniciales
betas <- solve(t(X)%*%X)%*%t(X)%*%Y   ### EMV
countB = 0

### Prior
### betas ~ Normal(be0m,be0v)
be0m = rep(0,K)
be0v = diag(10000,K)

for(sim in 1:SIM){

### beta
	bv = diag(0.1,K)
	betasProposal <- t(rmnorm(n=1, mean=betas, varcov=bv))
	if(MHstepLogBeta(betas,betasProposal,Y,X,bv) >runif(1)){
		betas = betasProposal
		countB = countB+1
	}

### sample
	BETA31[sim,] = betas
}

summary(as.mcmc(BETA31))
plot(as.mcmc(BETA31))

##################################################
##################################################

