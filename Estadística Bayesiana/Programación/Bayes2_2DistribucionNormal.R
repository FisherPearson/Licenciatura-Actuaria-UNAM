##################################################
### Enfoque Bayesiano
### Distribucion Normal(mu,sigma^2) 
### varianza conocida
##################################################

library(VGAM)

##################################################
### Simular datos
### 
### Likelihood   Normal(theta,sigma2)  
n = 5
mu = 5
sigma.sq = 4
X = rnorm(n, mean=mu, sd=sqrt(sigma.sq))

### Prior   Normal(mu0,tau20)
mu0 = 4
tau0.sq = 1

#par(mfrow=c(1,2))
##################################################
### Posterior
tau1.sq = (1/tau0.sq + n/sigma.sq)^(-1)
mu1 = tau1.sq*(mu0/tau0.sq + n*mean(X)/sigma.sq)

theta = seq(min(X)-5*sd(X),max(X)+5*sd(X),length.out=100)   ### para graficar


plot( theta , dnorm(theta, mean=mu1, sd=sqrt(tau1.sq)), 
      main="Posterior distribution",xlab=expression(theta), ylab="Densidad", lty=1, lwd=4, type="l", col="blue",
      ylim=c(0,1.3),xlim=c(0,12))  ### posterior
lines(theta, dnorm(theta, mean=mean(X), sd=sqrt(sigma.sq/n)), lty=2, lwd=4, col="black")   ### verosimilitud
lines(theta, dnorm(theta,mean=mu0, sd=sqrt(tau0.sq)), lty=3, lwd=4, col="red")   ### prior
legend("topright", legend=c("Prior","Verosimilitud","Posterior"), lty=c(1,2,3), col=c("red","black","blue"), lwd=2)

##################################################
### Predictiva  Prior & Posterior

X.new = seq(mu-5*sqrt(sigma.sq),mu+5*sqrt(sigma.sq),length.out=1000)   ### para graficar

### predictive prior
mu0.new = mu0
tau0.new.sq = sigma.sq*tau0.sq*(1/tau0.sq+1/sigma.sq)

### predictive posterior
mu1.new = mu1
tau1.new.sq = sigma.sq*tau1.sq*(1/tau1.sq+1/sigma.sq)

plot(X.new,dnorm(X.new, mean=mu1, sd=sqrt(tau1.new.sq)), main="Predictive distribution",ylab="Densidad",type="l",lwd=2,col="blue")  ### posterior predictive 
lines(X.new, dnorm(X.new, mean=mu, sd=sqrt(sigma.sq)), lty=3, lwd=4, col="black")   ### verosimilitud
lines(X.new, dnorm(X.new, mean=mu0, sd=sqrt(tau0.new.sq)), lty=2, lwd=4, col="red")   ### prior predictive
legend("topleft", legend=c("Predictiva Prior","Modelo parametrico","Predictiva Posterior"), lty=c(1,3,2), col=c("red","black","blue"))  

##################################################

##################################################
