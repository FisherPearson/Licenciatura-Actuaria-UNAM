##################################################
### Monte Carlo Integration
##################################################
### Ejemplo Normal(0,1)

t = 1
pnorm(t)  # \Phi(t) fda de una Normal(0,1)

# usando integracion Monte Carlo
n = 1000
x = rnorm(n,0,1)
gx = ifelse(x<=t,1,0)
mean(gx)
var(gx) # = pnorm(t)*(1-pnorm(t))

##################################################

##################################################
### Monte Carlo Integration
### Importance Sampling

library(VGAM)

##################################################
### Ejemplo
### Calcular E[h(X)], donde X\sim Uniforme(0,10)
### h(X)=10\exp(-2 |x-5|)

### Opcion 1: simulando de U(0,10)
### Usando Integracion Monte Carlo

X = runif(1000,0,10)
hX = 10*exp(-2*abs(X-5))
c(mean(hX),var(hX))   # Integracion Monte Carlo


### Opcion 2: simulando de N(5,1)
### Usando Importance Sampling

X = rnorm(1000,5,1)
Y = 10*exp(-2*abs(X-5)) * dunif(X,0,10) /(dnorm(X,5,1))
c(mean(Y),var(Y))   # Importance Sampling

### NOTA: la varianza bajo el muestreo por Importancia es menor que usando directamente la Integración Monte Carlo. 
### Por tanto, se tiene mayor precisión acerca de la estimación. 
 
##################################################
### Ejemplo: Estimar los momentos E[X^k] donde X\sim Doble Exponencial o Laplace.
### f(x)= \frac{1}{2}\exp(-|x|)
### F(x) = \exp(x)/2I[x\leq0] + (1-\exp(-x)/2)I[x>0]
library(VGAM)
help(dlaplace)
 
### Opcion 1: simulando de Doble Exponencial
### Usando Integracion Monte Carlo

X = rlaplace(10000, location=0, scale=1)
Y = X^2   # X^k
c(mean(Y),var(Y))   # Integracion Monte Carlo

### Opcion 2: simulando de N(0,4)
### Usando Importance Sampling
X = rnorm(10000,0,2)
Y = (X^2) * dlaplace(X,0,1) /(dnorm(X,0,2))
#Y = (X^2) *(0.5*exp(-abs(X))) /(dnorm(X,0,2))
c(mean(Y),var(Y))   # Importance Sampling

##################################################

##################################################
### Sea $Z\sim Normal(0,1)$, calcular $P[Z>4.5]=3.398\times10^{-6}$
pnorm(-4.5)   # 
1-pnorm(4.5)   # 

### Opcion 1: Usando Integracion Monte Carlo
n = 100000 #Se requiere un numero grande de simulaciones. 

Z = rnorm(n)
sum(Z>4.5)/n # mean(Z>4.5)
plot(cumsum(Z)/1:n,type="l")
abline(a=pnorm(-4.5),b=0,col="red",lwd=2)

### Opcion 2: Usando Importance Sampling
n = 1000
x = rexp(n)+4.5
weit = dnorm(x)/dexp(x-4.5)
plot(cumsum(weit)/1:n,type="l")
abline(a=pnorm(-4.5),b=0,col="red",lwd=2)

mean(weit)
pnorm(-4.5)

##################################################


##################################################
