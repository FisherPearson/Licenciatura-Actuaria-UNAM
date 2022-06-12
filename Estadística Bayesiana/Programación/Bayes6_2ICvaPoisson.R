##################################################

### Intervalos de Confianza
### v.a. Discretas

##################################################

### Poisson

p1 = 0.0374
p2 = 0.0119
1-p1-p2   ### nivel de confianza
n = 10   ### tamaño de muestra
t = 8   ### estadistica suficiente


theta = seq(0,5,by=0.1)
th1 = ppois(t,n*theta)
th2 = 1-ppois(t-1,n*theta)

cbind(theta,round(th1,4))
cbind(theta,round(th2,4))


### Estimacion Puntual
t/n
### Solucion: theta \in (0.3,1.5) 


##################################################

