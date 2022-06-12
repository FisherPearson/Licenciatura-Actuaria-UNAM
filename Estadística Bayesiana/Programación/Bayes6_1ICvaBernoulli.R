##################################################

### Intervalos de Confianza
### v.a. Discretas

##################################################
### Bernoulli
p1 = 0.0509
p2 = 0.0159 
1-p1-p2   ### nivel de confianza
n = 20   ### tamaño de muestra
t = 4   ### estadistica suficiente

theta = seq(0,1,by=0.01)
th1 = pbinom(t,size=n,prob=theta)
th2 = 1-pbinom(t-1,size=n,prob=theta)

cbind(theta,round(th1,4))
cbind(theta,round(th2,4))

plot(theta,round(th1,4),type="l",xlab=expression(theta),ylab="p1")
abline(h=p1,col="blue",lwd=2)
plot(theta,round(th2,4),type="l",xlab=expression(theta),ylab="p2")
abline(h=p2,col="blue",lwd=2)

### Estimacion Puntual
t/n
### Solucion: theta \in (0.05,0.4)

##################################################
