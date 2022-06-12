##################################################
### Enfoque Bayesiano
##################################################

library(VGAM)
k = 10   ### tamaño de muestra 
a = 2   ### hiperparametro de Beta
b = 8   ### hiperparametro de Beta 

### Una sola muestra observada

p = seq(0,1,0.01)
YY = c(0,2,round(k/2),k)   

### Posterior

par(mfrow=c(2,2))
for(j in 1:4){
	y = YY[j]
	plot(p,dbeta(p,a,b), main=paste0("Muestra Y=",y),xlab=expression(theta), ylab="Densidad", lty=1, lwd=4, type="l", ylim=c(0,10), col="red")
	lines(p, dbeta(p,y+1,k-y+1), lty=2, lwd=4, col="black")
	lines(p, dbeta(p,a+y,b+k-y), lty=3, lwd=4, col="blue")
	legend("topright", legend=c("Prior","Verosimilitud","Posterior"), lty=c(1,2,3), col=c("red","black","blue"), lwd=2)
}
##################################################
#dev.new(width=4,height=4) 

par(mfrow=c(2,2))

### Likelihood   Binomial(y|k,p)  
### Prior   Beta(p|,a,b)


### Predictiva  Prior & Posterior

x = (0:k)
par(mfrow=c(2,2))
for(j in 1:4){
	y = YY[j]
	plot(x,dbetabinom.ab(x, size=k, shape1=a, shape2=b), main=paste0("Muestra Y=",y),xlab="Y",ylab="Densidad",type="h",lwd=2, ylim=c(0,0.3))
	points(x,dbetabinom.ab(x, size=k, shape1=a+y, shape2=b+k-y), col="blue",pch=19,cex=1.5)
	legend("topleft", legend=c("Predictiva Prior","Predictiva Posterior"), lty=c(1,NA), col=c("black","blue"), pch=c(NA,19))
}

##################################################

##################################################

#dev.new(width=4,height=4) 
par(mfrow=c(2,2))

### Likelihood   Binomial(y|k,p)  
### Prior   Beta(p|,a,b)
k = 10
a = 2
b = 2

### Muestra observada de tamanio n
n = 2
p = seq(0,1,0.01)
YY = matrix(NA,4,n)
YY[1,] = rep(0,n) 
YY[2,] = rep(2,n)
YY[3,] = round(runif(n)*k) 
YY[4,] = rep(k-1,n)

### Posterior

for(j in 1:4){
	y = YY[j,]
	plot(p,dbeta(p,a,b), main=paste0("Muestra suma Y=",sum(y)," media Y=",mean(y)),xlab=expression(theta), ylab="Densidad", lty=1, lwd=4, type="l", ylim=c(0,7), col="red")
	lines(p, dbeta(p,sum(y)+1,n*k-sum(y)+1), lty=2, lwd=4)
	lines(p, dbeta(p,a+sum(y),b+n*k-sum(y)), lty=3, lwd=4, col="blue")
	legend("topright", legend=c("Prior","Verosimilitud","Posterior"), lty=c(1,2,3), col=c("red","black","blue"), lwd=2)
}

### Predictiva  Prior & Posterior

x = (0:k)
for(j in 1:4){
	y = YY[j,]
	plot(x,dbetabinom.ab(x, size=k, shape1=a, shape2=b), main=paste0("Muestra suma Y=",sum(y)," media Y=",mean(y)),xlab="Y",ylab="Densidad",type="h",lwd=2, ylim=c(0,0.3))
	points(x,dbetabinom.ab(x, size=k, shape1=a+sum(y), shape2=b+n*k-sum(y)), col="blue",pch=19,cex=1.5)
	legend("topleft", legend=c("Predictiva Prior","Predictiva Posterior"), lty=c(1,NA), col=c("black","blue"), pch=c(NA,19))
}

##################################################

